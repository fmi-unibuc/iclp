defmodule ClientThread do
  def init() do
    receive do
      message -> IO.puts "Received: " <> inspect(message)
    end
    init()
  end
end
