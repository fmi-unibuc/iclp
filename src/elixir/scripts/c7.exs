defmodule Hello do
  def run() do
    me = self()
    send(me, :Hello!)
    send(me, "Hi!")
    receive do
      message when is_bitstring(message) ->
        IO.puts "Received: " <> message
      after
        1000 -> IO.puts "Timed out"
    end
    receive do
      :Hello! ->
        IO.puts "Hello to you, too!"
    end

  end
end







defmodule SelectiveReceive do
  def run() do
    me = self()
    send(me, :one)
    send(me, :two)
    send(me, :three)
    receive do
      :ionel -> IO.puts "Received ionel."
    end
    receive do
      :three -> IO.puts "Received three."
      :two -> IO.puts "Received two."
    end
    receive do
      :three -> IO.puts "Received three."
      :two -> IO.puts "Received two."
    end
    receive do
      :one -> IO.puts "Received one."
    end
  end
end

defmodule Pong do
  def init() do
    receive do
      {:ping, from} -> send from, :pong
    end
    init()
  end
end

defmodule PongTest do
  def run() do
    server = spawn(Pong, :init, [])
    me = self()

    send server, {:ping, me}
    receive do
      :pong -> IO.puts "received pong"
    end

    send server, {:ring, me}
    receive do
      :pong -> IO.puts "received pong again"
    after
      1000 -> IO.puts :stderr, "Timed out waiting for pong"
    end
  end
end

defmodule Stack do
  def init(), do: loop([])
  defp loop(state) do
    receive do
      {:push, value} -> loop([value | state])
      {:pop, reply} ->
        case state do
          [h | t] ->
            send(reply, {:some, h})
            loop(t)
          []      ->
            send(reply, {:none})
            loop([])
        end
    end
  end
end

defmodule StackAction do
  def init(), do: loop([])
  defp loop(state) do
    receive do
      {:push, value} -> loop([value | state])
      {:pop, action} -> case state do
          [h | t] -> action.(h)   ;  loop(t)
          []      -> action.(nil) ;  loop([])
        end
    end
  end
end

