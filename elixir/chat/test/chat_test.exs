defmodule ChatTest do
  use ExUnit.Case
  doctest Chat

  @tag :distributed
  test "send_message" do
    assert Chat.send_message(:cxs@localhost, "hi") == :ok
  end
end
