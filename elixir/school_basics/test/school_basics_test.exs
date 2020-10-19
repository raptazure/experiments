defmodule SchoolBasicsTest do
  use ExUnit.Case
  doctest SchoolBasics

  test "greets the world" do
    assert SchoolBasics.hello() == :world
  end
end
