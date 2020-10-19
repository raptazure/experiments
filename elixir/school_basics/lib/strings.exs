# Using « » syntax we are saying to the compiler that the elements inside those symbols are bytes.

# "hello"
string = <<104, 101, 108, 108, 111>>
# <<104, 101, 108, 108, 111, 0>>
string <> <<0>>

# [104, 101, 322, 322, 111]
'hełło'
# <<104, 101, 197, 130, 197, 130, 111, 0>>
"hełło" <> <<0>>

string = "\u0061\u0301"
# ["a", "́"]
String.codepoints(string)
# ["á"]
String.graphemes(string)

defmodule Anagram do
  def anagrams?(a, b) when is_binary(a) and is_binary(b) do
    sort_string(a) == sort_string(b)
  end

  def sort_string(string) do
    string
    |> String.downcase()
    |> String.graphemes()
    |> Enum.sort()
  end
end
