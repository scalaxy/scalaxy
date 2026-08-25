defmodule Escalaxy do
  @moduledoc """
  Official Elixir client SDK for [Scalaxy](https://scalaxy.org), the
  S3-backed distributed graph database.

  ## Quickstart

      client = Escalaxy.Client.new(base_url: "http://localhost:8080", db: "mydb")

      {:ok, result} = Escalaxy.query(client, "CREATE (:Person {name: \"Ada\"})")
      {:ok, result} = Escalaxy.query(client, "MATCH (p:Person) RETURN p.name")
      Enum.to_list(result.records)
      #=> [%{"p.name" => "Ada"}]

  ## Conventions

  * `query/3` returns `{:ok, %Escalaxy.Result{}}` or `{:error, Exception.t()}`.
  * `query!/3` returns the result directly and raises on failure.
  * Every call emits `[:escalaxy, :query, :stop]` telemetry with
    `:duration`, `:status` and `:db` measurements/metadata.

  See `Escalaxy.Graph` for helpers that build Cypher safely from Elixir
  values, and `Escalaxy.Ash.DataLayer` for Ash Framework integration.
  """

  alias Escalaxy.{Client, ConnectionError, Error, Result}

  @doc """
  Runs a Cypher query against Scalaxy.

  ## Options

    * `:timeout` - override the client's request timeout (ms).
    * `:params`   - map of Cypher parameters (`$name`) for the query.

  ## Examples

      {:ok, result} = Escalaxy.query(client, "MATCH (n) RETURN count(n)")
      Escalaxy.scalar(result) #=> 42
  """
  @spec query(Client.t(), String.t(), keyword()) ::
          {:ok, Result.t()} | {:error, Exception.t()}
  def query(%Client{} = client, cypher, opts \\ []) when is_binary(cypher) do
    timeout = Keyword.get(opts, :timeout, client.timeout)
    url = Client.cypher_url(client)

    payload = %{query: cypher}
    payload = if params = Keyword.get(opts, :params), do: Map.put(payload, :params, params), else: payload
    body = Jason.encode!(payload)
    start = System.monotonic_time()

    request =
      Finch.build(:post, url, [{"content-type", "application/json"}] ++ client.headers, body)

    case Finch.request(request, client.pool, receive_timeout: timeout) do
      {:ok, %Finch.Response{status: status, body: body}} when status in 200..299 ->
        with {:ok, decoded} <- decode(body) do
          emit(start, status, client.db)
          {:ok, Result.build(decoded["columns"] || [], decoded["rows"] || [])}
        end

      {:ok, %Finch.Response{status: status, body: body}} ->
        emit(start, status, client.db)
        message = decode_error(body)
        {:error, Error.exception(message: message, status: status, query: cypher)}

      {:error, exception} ->
        emit(start, nil, client.db)
        {:error, ConnectionError.exception(message: Exception.message(exception))}
    end
  end

  @doc """
  Like `query/3` but raises `Escalaxy.Error` or `Escalaxy.ConnectionError`
  on failure. Returns the `%Escalaxy.Result{}`.
  """
  @spec query!(Client.t(), String.t(), keyword()) :: Result.t()
  def query!(client, cypher, opts \\ []) do
    case query(client, cypher, opts) do
      {:ok, result} -> result
      {:error, exception} -> raise exception
    end
  end

  @doc "Returns the single scalar of a one-row one-column query (`nil` if empty)."
  @doc since: "0.1.0"
  @spec scalar(Result.t()) :: term()
  defdelegate scalar(result), to: Result

  @doc """
  Health-checks a node. Returns `:ok` or raises `Escalaxy.ConnectionError`.
  """
  @spec ping(Client.t()) :: :ok | {:error, ConnectionError.t()}
  def ping(%Client{} = client) do
    url = "#{client.base_url}/healthz"

    case Finch.request(Finch.build(:get, url), client.pool, receive_timeout: client.timeout) do
      {:ok, %Finch.Response{status: status}} when status in 200..299 -> :ok
      {:ok, %Finch.Response{status: status}} -> {:error, ConnectionError.exception(message: "unhealthy node (HTTP #{status})")}
      {:error, e} -> {:error, ConnectionError.exception(message: Exception.message(e))}
    end
  end

  ## internals

  defp decode(body) do
    case Jason.decode(body) do
      {:ok, map} when is_map(map) -> {:ok, map}
      {:ok, other} -> {:error, Error.exception(message: "unexpected response shape: #{inspect(other)}")}
      {:error, e} -> {:error, Error.exception(message: "invalid JSON from server: #{Exception.message(e)}")}
    end
  end

  defp decode_error(body) do
    case Jason.decode(body) do
      {:ok, %{"error" => msg}} when is_binary(msg) -> msg
      _ -> "query failed"
    end
  end

  defp emit(start, status, db) do
    :telemetry.execute([:escalaxy, :query, :stop], %{duration: System.monotonic_time() - start}, %{
      status: status,
      db: db
    })
  end
end
