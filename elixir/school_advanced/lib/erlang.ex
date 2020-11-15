defmodule Example do
  def timed(fun, args) do
    {time, result} = :timer.tc(fun, args)
    IO.puts("Time: #{time} μs")
    IO.puts("Result: #{result}")
  end
end

# png =
#   :png.create(%{:size => {30, 30}, :mode => {:indexed, 8}, :file => file, :palette => palette})

"Hello World"
|> to_charlist()
|> :string.words()
|> IO.puts()
