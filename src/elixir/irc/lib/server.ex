defmodule Server do
  @moduledoc """
  A server maintaining the list of channels and users
  """

  @doc """
  """
  def init() do
    general = spawn_link(Channel, :init, [:general])
    loop(%ServerState{
      me: self(),
      users: spawn_link(Storage, :init, []) ,
      channels: spawn_link(Storage, :init, [%{:general => general}]),
      userThreads: spawn_link(Storage, :init, []),
    })
  end

  @spec loop(ServerState.t()) :: any()
  defp loop(state) do
    receive do
      {:register, name, address} ->
        send(state.users, {:put, name, address,
          &handleRegistration(state, name, address, &1)
        })
      {:registered, name, address, thread, userChannels} ->
        ref = Process.monitor(thread)
        send(state.userThreads, {:put!, ref, {thread, name, userChannels}})
        send(address, {:registered, name, state.me, thread})
      {:DOWN, ref, :process, _thread, _reason} ->
        send(state.userThreads, {:get, ref, &logout(state, ref, &1)})
      message -> IO.inspect message
    end
    loop(state)
  end

  @spec handleRegistration(ServerState.t(), atom(), Process.dest(), atom()) :: any()
  defp handleRegistration(state, name, address, answer) do
    case answer do
      :ok ->
        spawn(fn -> ServerThread.init(state.me, state.channels, name, address) end)
      :not_ok -> send(address, {:not_registered, name, state.me})
    end
  end

  defp logout(_state, ref, nil),
    do: IO.puts "[server]: invalid_reference #{ref}; ignored"
  defp logout(state, ref, {_thread, name, userChannels}) do
    send(userChannels, {:foreach, &send(&1, {:leave, name})})
    send(state.userThreads, {:delete, ref})
    send(state.users, {:delete, name})
  end
end
