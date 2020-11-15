# ~C 创建一个不处理插值和转义字符的字符列表
# ~c 创建一个处理插值和转义字符的字符列表
# ~R 创建一个不处理插值和转义字符的正则表达式
# ~r 创建一个处理插值和转义字符的正则表达式
# ~S 创建一个不处理插值和转义字符的字符串
# ~s 创建一个处理插值和转义字符的字符串
# ~W 创建一个不处理插值和转义字符的单词列表
# ~w 创建一个处理插值和转义字符的单词列表
# ~N 创建一个 NaiveDateTime 格式的数据结构
# ~U 创建一个 DateTime 格式的数据结构 (Elixir 1.9.0 开始支持)

string = "100_000_000"
Regex.split(~r/_/, string)

IO.puts(NaiveDateTime.from_iso8601("2015-01-23 23:50:07") == {:ok, ~N[2015-01-23 23:50:07]})

IO.puts(
  DateTime.from_iso8601("2015-01-23 23:50:07-0600") == {:ok, ~U[2015-01-24 05:50:07Z], -21600}
)

# creating sigils
defmodule MySigils do
  def sigil_o(string, []), do: String.upcase(string)
end
