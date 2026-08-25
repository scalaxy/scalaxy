defmodule Escalaxy.Graph do
  @moduledoc """
  Helpers that build Cypher from plain Elixir values - safely.

  Strings are single-quote escaped per Scalaxy's Cypher dialect, numbers,
  booleans and `nil` are inlined as literals, lists become Cypher lists
  and maps become map literals. Use these when you cannot use parameters.

      iex> Escalaxy.Graph.literal("O'Brien")
      "'O\\\\'Brien'"

      iex> Escalaxy.Graph.map_literal(%{name: "Ada", age: 36})
      "{age: 36, name: 'Ada'}"
  """

  @type literal :: String.t() | number() | boolean() | nil | list() | map()

  @doc "Renders an Elixir value as a Cypher literal."
  @spec literal(literal()) :: String.t()
  def literal(value)

  def literal(nil), do: "null"
  def literal(true), do: "true"
  def literal(false), do: "false"
  def literal(v) when is_integer(v), do: Integer.to_string(v)
  def literal(v) when is_float(v), do: Float.to_string(v)
  def literal(v) when is_binary(v), do: "'" <> String.replace(v, "'", "\\'") <> "'"

  def literal(v) when is_list(v) do
    "[" <> Enum.map_join(v, ", ", &literal/1) <> "]"
  end

  def literal(v) when is_map(v), do: map_literal(v)

  @doc "Renders a map as a Cypher map literal (keys unquoted identifiers)."
  @spec map_literal(map()) :: String.t()
  def map_literal(map) when is_map(map) do
    inner =
      map
      |> Enum.sort_by(fn {k, _} -> to_string(k) end)
      |> Enum.map_join(", ", fn {k, v} -> "#{key(k)}: #{literal(v)}" end)

    "{" <> inner <> "}"
  end

  @doc "Builds a `CREATE` for a labeled node with properties."
  @spec create_node(String.t() | [String.t()], map()) :: String.t()
  def create_node(labels, props \\ %{}) when is_list(props) or is_map(props) do
    "CREATE (#{label_expr(labels)} #{literal(props)})" |> String.trim_trailing()
  end

  @doc "Builds a `MATCH ... WHERE id = ... DELETE` for a node by property."
  @spec delete_node(String.t(), atom() | String.t(), literal()) :: String.t()
  def delete_node(label, key, value) do
    "MATCH (n #{label_expr(label)}) WHERE n.#{key(key)} = #{literal(value)} DELETE n"
  end

  defp label_expr(labels) when is_list(labels),
    do: labels |> Enum.map(&key/1) |> Enum.join(":")

  defp label_expr(label), do: key(label)

  defp key(k) do
    k = to_string(k)
    if Regex.match?(~r/^[A-Za-z_][A-Za-z0-9_]*$/, k), do: k, else: "`" <> k <> "`"
  end
end
