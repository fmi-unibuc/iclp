defmodule IrcTest do
  use ExUnit.Case

  test "registration" do
    {:ok, server} = Server.start_link()
    {:registered, _} = Server.register(server, :name)
    assert_receive {:post, :general, "name has joined the channel"}

    assert Server.register(server, :name) == :name_already_registered
  end

  test "interaction" do
    {:ok, server} = Server.start_link()

    {:registered, _observerThread} = Server.register(server, :observer)
    assert_receive {:post, :general, "observer has joined the channel"}

    {:registered, userThread} = Server.register(server, :user)
    assert_receive {:post, :general, "user has joined the channel"}
    assert_receive {:post, :general, "user has joined the channel"}

    ServerThread.post(userThread, :general, "Salut!")
    assert_receive {:post, :general, {:user, "Salut!"}}
    assert_receive {:post, :general, {:user, "Salut!"}}

    GenServer.stop(userThread)

    assert_receive {:post, :general, "user has left the channel"}
  end

  test "supervisor" do
    {:ok, supervisor} = ServerSupervisor.start_link()
    Server.register(Server, :user)
    assert_receive {:post, :general, "user has joined the channel"}
    pid = Process.whereis(Server)
    Process.exit(pid, :kill)
    Process.sleep(1)
    Server.register(Server, :user)
    assert_receive {:post, :general, "user has joined the channel"}
    :ok = Supervisor.terminate_child(supervisor, Server)
    {:ok, _} = Supervisor.restart_child(supervisor, Server)
    Server.register(Server, :user)
    assert_receive {:post, :general, "user has joined the channel"}
  end
end
