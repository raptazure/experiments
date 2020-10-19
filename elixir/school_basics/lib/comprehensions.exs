import Integer

list = [1, 2, 3, 4]

for x <- list, do: x * x
for {_key, val} <- [one: 1, two: 2], do: val
for {k, v} <- %{"a" => "A", "b" => "B"}, do: {k, v}
for <<c <- "hello">>, do: <<c>>

for {:ok, val} <- [ok: "Hello", error: "Unknown", ok: "World"], do: val

for n <- list, times <- 1..n do
  IO.puts("#{n} - #{times}")
  String.duplicate("*", times)
  # ["*", "*", "**", "*", "**", "***", "*", "**", "***", "****"]
end

for x <- 1..10, is_even(x), rem(x, 3) == 0, do: x

for {k, v} <- [one: 1, two: 2], into: %{}, do: {k, v}
for c <- [72, 101, 108, 108, 111], into: "", do: <<c>>
