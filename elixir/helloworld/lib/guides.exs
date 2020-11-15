defmodule Math do
  def zero?(0) do
    true
  end

  def zero?(x) when is_integer(x) do
    false
  end

  # Enum.reduce([1, 2, 3], 0, fn x, acc -> x + acc end)
  # Enum.reduce(1..3, 0, &+/2)

  def sum_list([head | tail], accumulator) do
    sum_list(tail, head + accumulator)
  end

  def sum_list([], accumulator) do
    accumulator
  end

  # Enum.map(1..3, fn x -> x * 2 end)
  def double_each([head | tail]) do
    [head * 2 | double_each(tail)]
  end

  def double_each([]) do
    []
  end
end

IO.puts(Math.sum_list([1, 2, 3], 0))

defmodule Concat do
  def join(a, b \\ nil, sep \\ "")

  def join(a, b, _sep) when is_nil(b) do
    a
  end

  def join(a, b, sep) do
    a <> sep <> b
  end
end

# => Hello world
IO.puts(Concat.join("Hello", "world"))
# => Hello_world
IO.puts(Concat.join("Hello", "world", "_"))
# => Hello
IO.puts(Concat.join("Hello"))

defmodule Recursion do
  def print_multiple_times(msg, n) when n <= 1 do
    IO.puts(msg)
  end

  def print_multiple_times(msg, n) do
    IO.puts(msg)
    print_multiple_times(msg, n - 1)
  end
end

Recursion.print_multiple_times("Elixir", 3)

odd? = &(rem(&1, 2) != 0)

1..100_000
|> Enum.map(&(&1 * 3))
|> Enum.filter(odd?)
|> Enum.sum()
|> IO.puts()

# Streams are lazy, composable enumerables.
1..100_000
|> Stream.map(&(&1 * 3))
|> Stream.filter(odd?)
|> Enum.sum()

send(self(), {:h, "w"})

receive do
  {:h, msg} -> msg
after
  1000 ->
    "nothing after 1s"
end
