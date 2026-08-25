defmodule Escalaxy.GraphTest do
  use ExUnit.Case, async: true
  doctest Escalaxy.Graph

  alias Escalaxy.Graph

  describe "literal/1" do
    test "escapes single quotes" do
      assert Graph.literal("O\'Brien") == "\'O\\\'Brien\'"
    end

    test "primitives" do
      assert Graph.literal(nil) == "null"
      assert Graph.literal(true) == "true"
      assert Graph.literal(42) == "42"
      assert Graph.literal(3.5) == "3.5"
    end

    test "lists and maps" do
      assert Graph.literal([1, "a", nil]) == "[1, \'a\', null]"
      assert Graph.map_literal(%{b: 2, a: "x"}) == "{a: \'x\', b: 2}"
    end
  end

  describe "create_node/2" do
    test "labels and props" do
      assert Graph.create_node("Person", %{name: "Ada"}) ==
               "CREATE (Person {name: \'Ada\'})"

      assert Graph.create_node([:Zone, :Place], %{}) == "CREATE (Zone:Place {})"
    end

    test "weird identifiers are backtick-quoted" do
      assert Graph.create_node("My Label", %{}) == "CREATE (`My Label` {})"
    end
  end

  describe "delete_node/3" do
    test "builds match-delete" do
      assert Graph.delete_node("Person", :name, "Ada") ==
               "MATCH (n Person) WHERE n.name = \'Ada\' DELETE n"
    end
  end
end
