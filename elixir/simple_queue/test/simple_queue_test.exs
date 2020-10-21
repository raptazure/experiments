defmodule SimpleQueueTest do
  use ExUnit.Case
  doctest SimpleQueue

  test "greets the world" do
    assert SimpleQueue.hello() == :world
  end
end
