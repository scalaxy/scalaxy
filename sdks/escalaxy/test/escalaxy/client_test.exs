defmodule Escalaxy.ClientTest do
  use ExUnit.Case, async: true

  alias Escalaxy.Client

  test "new/1 applies defaults" do
    c = Client.new()
    assert c.base_url == "http://localhost:8080"
    assert c.db == "default"
    assert c.timeout == 15_000
  end

  test "cypher_url/1 encodes the db name" do
    c = Client.new(base_url: "http://node:8080/", db: "taxi live")
    assert Client.cypher_url(c) == "http://node:8080/api/cypher?db=taxi+live"
  end

  test "rejects unknown options" do
    assert_raise(ArgumentError, fn -> Client.new(bogus: 1) end)
  end
end
