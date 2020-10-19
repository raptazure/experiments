defmodule Example do
  @greeting "Hello"

  def greeting(name) do
    ~s(#{@greeting} #{name}.)
  end
end

defmodule Example.User do
  defstruct name: "Sean", roles: []
end

defmodule Example.User2 do
  @derive {Inspect, only: [:name]}
  defstruct name: nil, roles: []
end

defmodule Sayings.Greetings do
  def basic(name), do: "Hi, #{name}"
end

defmodule Example2 do
  alias Sayings.Greetings, as: Hi

  def greeting(name), do: Hi.basic(name)
end

defmodule Hello do
  defmacro __using__(opts) do
    greeting = Keyword.get(opts, :greeting, "Hi")

    quote do
      def hello(name), do: unquote(greeting) <> ", " <> name
    end
  end
end

defmodule Example3 do
  use Hello, greeting: "Hola"
end
