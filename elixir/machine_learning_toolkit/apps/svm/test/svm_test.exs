defmodule SvmTest do
  use ExUnit.Case
  doctest Svm

  test "greets the world" do
    assert Svm.hello() == :world
  end
end
