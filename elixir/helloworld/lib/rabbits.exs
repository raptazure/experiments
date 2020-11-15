defmodule Helper do
  def print_multiple_times(msg, n) when n <= 1 do
    IO.write(msg)
  end

  def print_multiple_times(msg, n) do
    IO.write(msg)
    print_multiple_times(msg, n - 1)
  end
end

defmodule Rabbit do
  def recursive_rb(n) do
    IO.write("\(\\_/)\n")
    Helper.print_multiple_times(" ", n)
    IO.write("( •_•)\n")
    Helper.print_multiple_times(" ", n)
    IO.write("/>")
    Process.sleep(1_000)
    recursive_rb(n + 1)
  end
end

IO.write(" ")
Rabbit.recursive_rb(1)
