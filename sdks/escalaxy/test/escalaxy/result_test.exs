defmodule Escalaxy.ResultTest do
  use ExUnit.Case, async: true

  alias Escalaxy.Result

  test "builds records as maps of string keys" do
    r = Result.build(["id", "name"], [[1, "ada"], [2, "grace"]])
    assert r.columns == ["id", "name"]
    assert Enum.to_list(r.records) == [%{"id" => 1, "name" => "ada"}, %{"id" => 2, "name" => "grace"}]
  end

  test "scalar/1" do
    assert Result.scalar(Result.build(["count"], [[7]])) == 7
    assert Result.scalar(Result.build(["count"], [])) == nil
  end

  test "handles ragged rows gracefully" do
    r = Result.build(["a", "b"], [[1]])
    assert Enum.to_list(r.records) == [%{"a" => 1}]
  end
end
