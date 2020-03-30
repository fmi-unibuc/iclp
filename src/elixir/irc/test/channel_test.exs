defmodule ChannelTest do
  use ExUnit.Case
  doctest Channel

  test "channel test" do
    channel = spawn(Channel, :init, [:mychannel])
    void = spawn(fn -> 5 end)
    me = self()
    send(channel, {:join, :observer, me})
    assert_receive {:post, :mychannel, "observer has joined the channel"}

    send(channel, {:join, :name, void})
    assert_receive {:post, :mychannel, "name has joined the channel"}

    send(channel, {:join, :name, void})

    send(channel, {:post, {:name, "Salut!"}})
    assert_receive {:post, :mychannel, {:name, "Salut!"}}

    send(channel, {:leave, :name})
    assert_receive {:post, :mychannel, "name has left the channel"}
  end
end
