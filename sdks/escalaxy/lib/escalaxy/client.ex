defmodule Escalaxy.Client do
  @moduledoc """
  An HTTP connection to a Scalaxy node or gateway.

    client = Escalaxy.Client.new(base_url: "http://localhost:8080", db: "mydb")

  The struct is immutable and cheap to copy; pass it around freely or
  store it in your own supervision tree. A default named pool is started
  by `Escalaxy.Application` and shared by all clients.
  """

  defstruct [:base_url, :db, :pool, :timeout, :headers]

  @type t :: %__MODULE__{
          base_url: String.t(),
          db: String.t(),
          pool: atom(),
          timeout: pos_integer(),
          headers: [{String.t(), String.t()}]
        }

  @defaults [
    base_url: "http://localhost:8080",
    db: "default",
    pool: Escalaxy.Finch,
    timeout: 15_000,
    headers: []
  ]

  @doc "Creates a client. Accepts `:base_url`, `:db`, `:pool`, `:timeout`, `:headers`."
  @spec new(keyword()) :: t()
  def new(opts \\ []) do
    opts = Keyword.validate!(opts, @defaults)
    %__MODULE__{
      base_url: String.trim_trailing(opts[:base_url], "/"),
      db: opts[:db],
      pool: opts[:pool],
      timeout: opts[:timeout],
      headers: opts[:headers]
    }
  end

  @doc "Returns the URL of the Cypher endpoint for this client."
  @spec cypher_url(t()) :: String.t()
  def cypher_url(%__MODULE__{} = client) do
    "#{client.base_url}/api/cypher?db=#{URI.encode_www_form(client.db)}"
  end
end
