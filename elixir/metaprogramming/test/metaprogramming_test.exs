defmodule MetaprogrammingTest do
  use ExUnit.Case
  doctest Metaprogramming

  test "greets the world" do
    assert Metaprogramming.hello() == :world
  end
end
