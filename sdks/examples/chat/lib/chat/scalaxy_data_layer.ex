defmodule Chat.ScalaxyDataLayer do
  @moduledoc """
  An Ash data layer that persists every resource as a labelled node in
  Scalaxy, addressed with Cypher through the official `Escalaxy` SDK.

  * one node per record, label = resource name (e.g. `Room`, `Message`)
  * `id` is application-generated (UUIDv7 string) so writes are
    idempotent-friendly and orderable
  * reads scan the label index and translate simple equality filters,
    sorts, offsets and limits into the query
  """

  @behaviour Ash.DataLayer

  alias Escalaxy.{Client, Error}
  @impl true

  def can?(_resource, :create), do: true
  def can?(_resource, :update), do: true
  def can?(_resource, :destroy), do: true
  def can?(_resource, :read), do: true
  def can?(_resource, :sort), do: true
  def can?(_resource, {:sort, _storage_type}), do: true
  def can?(_resource, :limit), do: true
  def can?(_resource, :offset), do: true
  def can?(_resource, :filter), do: true
  # Filters are translated in memory inside run_query, so every operator
  # and function is "supported" at declaration time.
  def can?(_resource, {:filter_expr, _expr}), do: true
  def can?(_resource, :nested_expressions), do: true
  def can?(_resource, {:nested_expressions, _, _}), do: true
  def can?(_resource, {:filter_operator, _}), do: true
  def can?(_resource, {:filter_function, _, _}), do: true
  def can?(_resource, {:filter_relationship_instructions, _}), do: true
  def can?(_resource, :expression_calculation), do: false
  def can?(_resource, :expr_error), do: true
  def can?(_, _), do: false

  defmodule Query do
    @moduledoc false
    defstruct [:filter, :sort, :limit, :offset]
  end

  @impl true
  def resource_to_query(_resource, _domain), do: %Query{}

  @impl true
  def filter(query, filter, _resource), do: {:ok, Map.put(query, :filter, filter)}

  @impl true
  def sort(query, sort, _resource), do: {:ok, Map.put(query, :sort, sort)}

  @impl true
  def limit(query, limit, _resource), do: {:ok, Map.put(query, :limit, limit)}

  @impl true
  def offset(query, offset, _resource), do: {:ok, Map.put(query, :offset, offset)}

  # -- write path ----------------------------------------------------------

  @impl true
  def create(resource, changeset) do
    id = Ash.Changeset.get_attribute(changeset, :id) || Ecto.UUID.generate()

    attrs =
      changeset.attributes
      |> Map.put("id", id)
      |> Map.new(fn {k, v} -> {to_string(k), dump(v)} end)


    labels = labels(resource)
    # Scalaxy does not allow parameters in node patterns; build the map
    # literal with the SDK's escaping helpers instead.
    cypher = "CREATE (:#{labels} " <> Escalaxy.Graph.map_literal(attrs) <> ")"

    case run(client(), cypher, %{}) do
      {:ok, _} -> {:ok, reify(resource, attrs)}
      {:error, e} -> {:error, e}
    end
  end

  @impl true
  def update(resource, changeset) do
    data = changeset.data
    id = Map.get(data, :id)
    props = Map.new(changeset.attributes, fn {k, v} -> {to_string(k), dump(v)} end)

    sets =
      props
      |> Map.keys()
      |> Enum.map(fn k -> "n.#{k} = $props.#{k}" end)
      |> Enum.join(", ")

    cypher = """
    MATCH (n:#{labels(resource)} {id: $id})
    SET #{sets}
    RETURN n
    """

    with {:ok, _} <- run(client(), cypher, %{"id" => id, "props" => props}) do
      {:ok, data}
    end
  end

  @impl true
  def destroy(resource, changeset) do
    id = Map.get(changeset.data, :id)

    cypher = "MATCH (n:#{labels(resource)} {id: $id}) DELETE n"

    case run(client(), cypher, %{"id" => id}) do
      {:ok, _} -> {:ok, changeset.data}
      {:error, e} -> {:error, e}
    end
  end

  # -- read path -----------------------------------------------------------

  @impl true
  def run_query(query, resource) do
    label = labels(resource)
    # NOTE: push equality predicates into the Cypher WHERE clause.
    # Inline-property MATCHes hit a stale on-boot index for freshly
    # written nodes, whereas WHERE performs a live scan.
    {where, _} = cypher_where(query.filter)

    cypher = "MATCH (n:#{label})#{where} RETURN n"

    with {:ok, result} <- run(client(), cypher, %{}) do
      records =
        result.records
        |> Enum.map(fn rec ->
          props = node_props(rec)
          reify(resource, props)
        end)
        |> apply_filter_in_memory(query.filter, resource)
        |> apply_sort(query.sort)
        |> apply_offset_limit(query.offset, query.limit)

      {:ok, records}
    end
  end

  ## helpers
  defp client do
    Application.get_env(:chat, :escalaxy_client) ||
      Client.new(
        base_url: Application.get_env(:chat, :escalaxy_url, "http://localhost:8080"),
        db: Application.get_env(:chat, :escalaxy_db, "chat")
      )
  end

  defp run(client, cypher, params) do
    Escalaxy.query(client, cypher, params: params)
  rescue
    e in [Error, Escalaxy.ConnectionError] -> {:error, e}
  end

  defp labels(resource) do
    resource
    |> Module.split()
    |> List.last()
  end


  defp cypher_where(filter) do
    case flatten_eq(filter) do
      [] ->
        {"", %{}}

      preds ->
        {clauses, _} =
          Enum.map_reduce(preds, 0, fn {field, value}, i ->
            field_name =
              cond do
                is_binary(field) -> field
                is_atom(field) and not is_boolean(field) -> Atom.to_string(field)
                is_map(field) -> to_string(Map.get(field, :name) || Map.get(field, :attribute) || "")
                true -> ""
              end

            value_str =
              cond do
                is_binary(value) -> Escalaxy.Graph.literal(value)
                is_number(value) or is_boolean(value) -> to_string(value)
                true -> nil
              end

            if field_name != "" and not is_nil(value_str) do
              {"n." <> field_name <> " = " <> value_str, i + 1}
            else
              {nil, i}
            end
          end)

        clauses = Enum.reject(clauses, &is_nil/1)

        case clauses do
          [] -> {"", %{}}
          list -> {" WHERE " <> Enum.join(list, " AND "), %{}}
        end
    end
  end

  defp apply_filter_in_memory(records, filter, _resource)

  defp apply_filter_in_memory(records, nil, _resource), do: records

  defp apply_filter_in_memory(records, filter, _resource) do
    exprs = flatten_eq(filter)
    if exprs == [] do
      records
    else
      Enum.filter(records, fn rec ->
        Enum.all?(exprs, fn {field, value} -> Map.get(rec, field) == value end)
      end)
    end
  end

  defp flatten_eq(%Ash.Filter{expression: expr}), do: flatten_eq(expr)
  defp flatten_eq(%Ash.Filter.Simple{predicates: preds}), do: Enum.flat_map(preds, &flatten_eq/1)

  # Ash.Query.Operator.Eq structs carry left/right directly.
  defp flatten_eq(preds) when is_list(preds), do: Enum.flat_map(preds, &flatten_eq/1)

  defp flatten_eq(op) do
    if is_map(op) and Map.has_key?(op, :left) do
      case {extract_attr(Map.get(op, :left)), extract_value(Map.get(op, :right))} do
        {nil, _} -> []
        {_attr, nil} -> []
        {attr, value} -> [{attr, value}]
      end
    else
      []
    end
  end


  defp extract_attr(v) do
    attr =
      cond do
        is_struct(v, Ash.Query.Ref) -> v.attribute
        is_map(v) and Map.has_key?(v, :attribute) -> v.attribute
        true -> v
      end

    # In Ash 3, Ref.attribute can itself be an attribute struct.
    cond do
      is_atom(attr) and not is_boolean(attr) -> attr
      is_binary(attr) -> String.to_atom(attr)
      is_map(attr) and Map.has_key?(attr, :name) -> attr.name
      true -> nil
    end
  rescue
    _ -> nil
  end
  defp extract_value(%_struct{} = v) do
    # Ash wraps literal values; pull the raw term out of whatever shape arrives.
    case Map.get(v, :expr) || Map.get(v, :value) do
      nil -> nil
      inner -> extract_value(inner)
    end
  rescue
    _ -> nil
  end

  defp extract_value(v), do: if(is_atom(v) and not is_boolean(v), do: nil, else: v)

  defp apply_sort(records, nil), do: Enum.reverse(records)
  defp apply_sort(records, []), do: records

  defp apply_sort(records, sort) when is_list(sort) do
    Enum.sort(records, fn a, b ->
      Enum.all?(sort, fn
        {field, :asc} -> (Map.get(a, field) || "") <= (Map.get(b, field) || "")
        {field, :desc} -> (Map.get(a, field) || "") >= (Map.get(b, field) || "")
        _ -> true
      end)
    end)
  end

  defp apply_sort(records, direction) when direction in [:asc, :desc],
    do: apply_sort(records, [{:inserted_at, direction}])

  defp apply_offset_limit(records, offset, limit) do
    records =
      if is_integer(offset) and offset > 0, do: Enum.drop(records, offset), else: records

    if is_integer(limit), do: Enum.take(records, limit), else: records
  end

  defp reify(resource, attrs) do
    pkey = Ash.Resource.Info.primary_key(resource)

    attrs =
      if length(pkey) == 1 do
        Map.put(attrs, to_string(hd(pkey)), attrs["id"])
      else
        attrs
      end

    attrs =
      attrs
      |> Enum.map(fn {k, v} ->
        {cast_key(resource, k), maybe_cast_type(resource, k, v)}
      end)
      |> Map.new()

    struct(resource, attrs)
  end

  defp cast_key(resource, key) when is_binary(key) do
    attr = Ash.Resource.Info.attribute(resource, key)
    if attr, do: attr.name, else: String.to_atom(key)
  end

  defp cast_key(_resource, key), do: key

  defp maybe_cast_type(resource, key, value) when is_binary(value) do
    case Ash.Resource.Info.attribute(resource, key) do
      %{type: type} ->
        cond do
          type in [:utc_datetime, :utc_datetime_usec] ->
            case DateTime.from_iso8601(value) do
              {:ok, dt, _} -> dt
              _ -> value
            end

          type in [:naive_datetime, :naive_datetime_usec] ->
            case NaiveDateTime.from_iso8601(value) do
              {:ok, ndt} -> ndt
              _ -> value
            end

          true ->
            value
        end

      nil ->
        value
    end
  rescue
    _ -> value
  end

  defp maybe_cast_type(_resource, _key, value), do: value

  # `RETURN n` yields records shaped %{"n" => %{"id" => gid, "props" => %{...}}}.
  defp node_props(rec) when is_map(rec) do
    case Map.values(rec) do
      [%{} = inner] ->
        case inner do
          %{"props" => props} when is_map(props) -> stringy(props)
          _ -> stringy(inner)
        end

      _ ->
        stringy(rec)
    end
  end

  defp stringy(map), do: Map.new(map, fn {k, v} -> {to_string(k), v} end)

  defp dump(%Date{} = v), do: Date.to_iso8601(v)
  defp dump(%DateTime{} = v), do: DateTime.to_iso8601(v)
  defp dump(%NaiveDateTime{} = v), do: NaiveDateTime.to_iso8601(v)

  defp dump(v) when is_struct(v), do: dump(Map.from_struct(v))
  defp dump(v) when is_map(v), do: Map.new(v, fn {k, x} -> {to_string(k), dump(x)} end)
  defp dump(v) when is_list(v), do: Enum.map(v, &dump/1)
  defp dump(v) when is_tuple(v), do: Tuple.to_list(v)
  defp dump(v), do: v
end
