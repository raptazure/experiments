defmodule Metaprogramming do
  def macro do
    # {:+, [context: Elixir, import: Kernel], [1, 2]}
    # quote do: 1 + 2
    # denominator = 2
    # {:divide, [], [42, {:denominator, [], Elixir}]}
    # quote do: divide(42, denominator)
    # {:divide, [], [42, 2]}
    # quote do: divide(42, unquote(denominator))
  end
end

defmodule OurMacro do
  defmacro unless(expr, do: block) do
    quote do
      if !unquote(expr), do: unquote(block)
    end
  end
end

# defmodule Logger do
#   defmacro log(msg) do
#     if Application.get_env(:logger, :enabled) do
#       quote do
#         IO.puts("logged message: #{unquote(msg)}")
#       end
#     end
#   end
# end

# defmodule Example do
#   require Logger

#   def tets do
#     Logger.log("This is a log msg")
#   end
# end

# Macro.to_string(quote(do: foo.bar(1, 2, 3)))

# quoted = quote do
#   OurMacro.unless(true, do: "Hi")
# end

# quoted
# |> Macro.expand_once(__ENV__)
# |> Macro.to_string
# |> IO.puts
# if(!true) do
#   "Hi"
# end

# defmodule Example do
#   defmacro hygienic do
#     quote do: val = -1
#   end

#   defmacro unhygienic do
#     quote do: var!(val) = -1
#   end
# end

# defmodule Example do
#   defmacro double_puts(expr) do
#     quote bind_quoted: [expr: expr] do
#       IO.puts(expr)
#       IO.puts(expr)
#     end
#   end
# end

# defmodule Example do
#   defmacro double_puts(expr) do
#     quote bind_quoted: [expr: expr] do
#       IO.puts(expr)
#       IO.puts(expr)
#     end
#   end
# end
