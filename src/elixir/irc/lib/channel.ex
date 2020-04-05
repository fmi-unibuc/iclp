defmodule Channel do
  use GenServer

  @doc """
  Starts the channel.
  """
  @spec start_link(atom(), [GenServer.option()]) :: GenServer.on_start()
  def start_link(channel_name, opts \\ []) do
    GenServer.start_link(__MODULE__, channel_name, opts)
  end

  def post(server, message) do
    GenServer.cast(server, {:post, message})
  end

  def join(server, name, address) do
    GenServer.cast(server, {:join, name, address})
  end

  def leave(server, name) do
    GenServer.cast(server, {:leave, name})
  end

  @impl true
  def init(channel_name) do
    {:ok, userAgent} = Storage.start_link()
    {:ok, %ChannelState{
      users: userAgent,
      name: channel_name,
    }}
  end

  @impl true
  def handle_cast({:post, message}, state) do
    Storage.foreach(state.users, &send(&1, {:post, state.name, message}))
    {:noreply, state}
  end

  @impl true
  def handle_cast({:join, name, address}, state) do
    case Storage.put(state.users, name, address) do
      :ok -> Channel.post(self(), "#{name} has joined the channel")
      :not_ok -> IO.puts :stderr, "#{name} already in the channel"
    end
    {:noreply, state}
  end

  @impl true
  def handle_cast({:leave, name}, state) do
    Storage.delete(state.users, name)
    Channel.post(self(), "#{name} has left the channel")
    {:noreply, state}
  end

end
