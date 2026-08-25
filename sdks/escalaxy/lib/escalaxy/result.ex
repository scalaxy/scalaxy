defmodule Escalaxy.Result do
  @moduledoc """
  The outcome of a successful query.

  * `columns` - column names as returned by Scalaxy.
  * `rows`    - raw row tuples.
  * `records` - convenience stream of column => value maps.

  ## Examples

      result.columns  #=> ["id"]
      result.rows     #=> [[1], [2]]
      Enum.to_list(result.records)  #=> [%{"id" => 1}, %{"id" => 2}]
  """

  defstruct [:columns, :rows, :records]

  @type t :: %__MODULE__{
          columns: [String.t()],
          rows: [[term()]],
          records: Enumerable.t()
        }

  @doc false
  @spec build([String.t()], [[term()]]) :: t()
  def build(columns, rows) when is_list(columns) and is_list(rows) do
    records =
      Stream.map(rows, fn row ->
        columns |> Enum.zip(row || []) |> Map.new(fn {k, v} -> {to_string(k), v} end)
      end)

    %__MODULE__{columns: columns, rows: rows, records: records}
  end

  @doc "Returns the single scalar value of a one-row/one-column query."
  @spec scalar(t()) :: term()
  def scalar(%__MODULE__{rows: [[value] | _]}), do: value
  def scalar(%__MODULE__{rows: []}), do: nil
end
