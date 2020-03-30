defmodule IrcTest do
  use ExUnit.Case

  test "registration" do
    server = spawn(Server, :init, [])
    me = self()
    send(server, {:register, :name, me})
    assert_receive {:post, :general, "name has joined the channel"}
    assert_receive {:registered, :name, ^server, _}

    send(server, {:register, :name, me})
    assert_receive {:not_registered, :name, ^server}
  end

  test "interaction" do
    server = spawn(Server, :init, [])
    me = self()

    send(server, {:register, :observer, me})
    assert_receive {:post, :general, "observer has joined the channel"}
    assert_receive {:registered, :observer, ^server, observerThread}

    send(server, {:register, :user, me})
    assert_receive {:post, :general, "user has joined the channel"}
    assert_receive {:post, :general, "user has joined the channel"}
    assert_receive {:registered, :user, ^server, userThread}

    send userThread, {:post, :general, "Salut!"}
    assert_receive {:post, :general, {:user, "Salut!"}}
    assert_receive {:post, :general, {:user, "Salut!"}}

    Process.exit(userThread, :kill)

    assert_receive {:post, :general, "user has left the channel"}

  end
end
