defmodule GenstageExampleTest do
  use ExUnit.Case
  doctest GenstageExample

  test "greets the world" do
    assert GenstageExample.hello() == :world
  end
end
