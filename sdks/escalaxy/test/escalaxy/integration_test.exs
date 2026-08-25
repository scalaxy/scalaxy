defmodule Escalaxy.IntegrationTest do
  use ExUnit.Case

  # Excluded by default (see test/test_helper.exs). Run against a live cluster:
  #
  #   SCALAXY_URL=http://localhost:8080 mix test --include integration
  @moduletag :integration

  alias Escalaxy.{Client, Error}

  setup do
    case System.get_env("SCALAXY_URL") do
      nil -> {:skip, "set SCALAXY_URL to run integration tests"}
      base -> [client: Client.new(base_url: base, db: "sdk-test")]
    end
  end

  test "roundtrip create/match/delete", %{client: client} do
    :ok = Escalaxy.ping(client)
    name = "sdk-" <> Integer.to_string(System.system_time())

    # NOTE: use WHERE clauses rather than inline property matches --
    # freshly written nodes are served by the live scan path immediately
    # (see scripts/KNOWN-ISSUES.md KI-1 in the Scalaxy repository).
    Escalaxy.query!(client, "CREATE (:SdkProbe {id: '#{name}'})")

    result =
      Escalaxy.query!(client, "MATCH (n:SdkProbe) WHERE n.id = '#{name}' RETURN count(n)")

    assert Escalaxy.scalar(result) == 1

    Escalaxy.query!(client, "MATCH (n:SdkProbe) WHERE n.id = '#{name}' DELETE n")

    result =
      Escalaxy.query!(client, "MATCH (n:SdkProbe) WHERE n.id = '#{name}' RETURN count(n)")

    assert Escalaxy.scalar(result) == 0
  end

  test "syntax errors return Escalaxy.Error", %{client: client} do
    assert {:error, %Error{}} = Escalaxy.query(client, "THIS IS NOT CYPHER")
  end

  test "connection errors are reported as Escalaxy.ConnectionError" do
    dead = Client.new(base_url: "http://127.0.0.1:59999", timeout: 500)
    assert {:error, %Escalaxy.ConnectionError{}} = Escalaxy.query(dead, "RETURN 1")
  end
end
