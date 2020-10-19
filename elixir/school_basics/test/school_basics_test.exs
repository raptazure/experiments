defmodule SchoolBasicsTest do
  use ExUnit.Case
  doctest SchoolBasics

  test "greets the world" do
    assert SchoolBasics.hello() == :world
  end
end

defmodule SendingProcess do
  def run(pid) do
    send(pid, :ping)
  end
end

defmodule TestReceive do
  use ExUnit.Case

  test "receives ping" do
    SendingProcess.run(self())
    assert_received :ping
  end
end

defmodule OutputTest do
  use ExUnit.Case
  import ExUnit.CaptureIO

  test "outputs Hello World" do
    assert capture_io(fn -> IO.puts("Hello World") end) == "Hello World\n"
  end
end

defmodule ExampleTest do
  use ExUnit.Case
  doctest SchoolBasics

  setup_all do
    {:ok, number: 2}
  end

  test "the truth", state do
    assert 1 + 1 == state[:number]
  end
end
