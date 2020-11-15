defmodule SchoolAdvancedTest do
  use ExUnit.Case
  doctest SchoolAdvanced

  test "greets the world" do
    assert SchoolAdvanced.hello() == :world
  end
end
