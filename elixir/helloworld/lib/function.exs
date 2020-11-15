defmodule Greeter do
  def hello(%{name: person_name} = person) do
    IO.puts("Hello, " <> person_name)
    IO.inspect(person)
  end
end

person = %{name: "Fred", age: "95", favorite_color: "Taupe"}

# Functions pattern-match the data passed in to each of its arguments independently. We can use this to bind values to separate variables within the function.
Greeter.hello(person)
Greeter.hello(%{name: "Free"})

defmodule Greeter2 do
  def hello(names) when is_list(names) do
    names
    |> Enum.join(", ")
    |> hello()
  end

  def hello(name) when is_binary(name) do
    phrase() <> name
  end

  defp phrase, do: "Hello, "
end

defmodule Greeter3 do
  def hello(name, language_code \\ "en") do
    phrase(language_code) <> name
  end

  defp phrase("en"), do: "Hello, "
  defp phrase("es"), do: "Hola, "
end

# Elixir doesn’t like default arguments in multiple matching functions. To handle this we add a function head with our default arguments.
defmodule Greeter4 do
  def hello(names, language_code \\ "en")

  def hello(names, language_code) when is_list(names) do
    names
    |> Enum.join(", ")
    |> hello(language_code)
  end

  def hello(name, language_code) when is_binary(name) do
    phrase(language_code) <> name
  end

  defp phrase("en"), do: "Hello, "
  defp phrase("es"), do: "Hola, "
end
