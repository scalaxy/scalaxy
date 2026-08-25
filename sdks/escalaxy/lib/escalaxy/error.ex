defmodule Escalaxy.Error do
  @moduledoc """
  A failed query. Carries the Scalaxy error message and, when known,
  the HTTP status and the original query text.
  """

  defexception [:message, :status, :query]

  @type t :: %__MODULE__{message: String.t(), status: integer() | nil, query: String.t() | nil}

  @impl true
  def message(%__MODULE__{message: msg, status: nil, query: nil}), do: msg
  def message(%__MODULE__{message: msg, status: s, query: nil}), do: "#{msg} (HTTP #{s})"
  def message(%__MODULE__{message: msg, status: s, query: q}) do
    "#{msg} (HTTP #{s}, query: #{truncate(q)})"
  end

  defp truncate(q), do: q |> String.replace("\n", " ") |> String.slice(0, 80)
end

defmodule Escalaxy.ConnectionError do
  @moduledoc "The Scalaxy node could not be reached or did not answer in time."
  defexception [:message]

  @type t :: %__MODULE__{message: String.t()}

  @impl true
  def message(%__MODULE__{message: msg}), do: msg
end
