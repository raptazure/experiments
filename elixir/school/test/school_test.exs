defmodule SchoolTest do
  use ExUnit.Case
  doctest School

  test "greets the world" do
    assert School.hello() == :world
  end
end
