defmodule Factorial do
  def of(n) when is_integer(n) and n > 0, do: n * of(n - 1)
  def of(_n), do: 1
end

1..6
|> Enum.map(fn i -> {i, Factorial.of(i)} end)
|> Enum.map(fn {i, fac} -> "Factorial of #{i}: #{fac}" end)
|> Enum.join("\n")
|> IO.puts()
