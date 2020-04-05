defmodule ChannelTest do
  use ExUnit.Case
  doctest Channel

  test "channel test" do
    {:ok, channel} = Channel.start_link(:mychannel)
    void = spawn(fn -> 5 end)
    me = self()
    Channel.join(channel,:observer, me)
    assert_receive {:post, :mychannel, "observer has joined the channel"}

    Channel.join(channel, :name, void)
    assert_receive {:post, :mychannel, "name has joined the channel"}

    Channel.join(channel, :name, void)

    Channel.post(channel, {:name, "Salut!"})
    assert_receive {:post, :mychannel, {:name, "Salut!"}}

    Channel.leave(channel, :name)
    assert_receive {:post, :mychannel, "name has left the channel"}
  end
end
