7 + 3

IO.puts "Salut"

defmodule MyBools do
# or true, _ -> true
  def myOr(true, _), do: true
# or false, orice -> orice
  def myOr(false, anything), do: anything

# || false, y -> y
  def myNSOr(false, anything), do: anything
# || nil,y -> y
  def myNSOr(nil, anything), do: anything
# || x,_ -> x
  def myNSOr(something, _), do: something
end

