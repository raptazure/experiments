defmodule ProtocolExample do
  # to_string({3.14, "apple", :pie})
  defimpl String.Chars, for: Tuple do
    def to_string(tuple) do
      interior =
        tuple
        |> Tuple.to_list()
        |> Enum.map(&Kernel.to_string/1)
        |> Enum.join(", ")

      "{#{interior}}"
    end
  end

  defprotocol AsAtom do
    def to_atom(data)
  end

  defimpl AsAtom, for: Atom do
    def to_atom(atom), do: atom
  end

  defimpl AsAtom, for: BitString do
    defdelegate to_atom(string), to: String
  end

  defimpl AsAtom, for: List do
    defdelegate to_atom(list), to: List
  end

  defimpl AsAtom, for: Map do
    def to_atom(map), do: List.first(Map.keys(map))
  end
end
