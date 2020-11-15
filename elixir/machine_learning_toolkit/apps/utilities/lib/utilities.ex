defmodule Utilities do
  # @spec sum_times(integer, %Examples{integer, integer}) :: integer
  @spec sum_times(integer, Examples.t()) :: integer
  def sum_times(a, params) do
    for i <- params.first..params.last do
      i
    end
    |> Enum.map(fn el -> el * a end)
    |> Enum.sum()
    |> round
  end
end

defmodule Example do
  defstruct first: nil, last: nil

  @type t(first, last) :: %Example{first: first, last: last}

  @typedoc """
      Type that represents Examples struct with :first as integer and :last as integer.
  """
  @type t :: %Example{first: integer, last: integer}
end
