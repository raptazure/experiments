defmodule TasksTest do
  use ExUnit.Case
  doctest Tasks

  test "greets the world" do
    assert Tasks.hello() == :world
  end
end
