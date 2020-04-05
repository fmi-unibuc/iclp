---
title: Implementarea Concurenței în limbaje de programare
subtitle: Processes, Servers callbacks, client APIs, and supervisors 
author: Traian Florin Șerbănuță
institute: FMI @ UNIBUC
abstract: |
---

# Process

### Creation: `spawn(fun, opts)` / `spawn(mod, fun, args, opts)`

Spawns the given function according to the given options


#### Spawn options

```elixir
@typedef spawn_opts :: [spawn_opt()]
@typedef spawn_opt() ::
```

`:link`

: links spawned process to parent, like `spawn_link`

`| :monitor`

: sets parent process as monitor for spawned one
: returns tuple `{pid, ref}`, like `spawn_monitor`

`| {:priority, :low | :normal | :high}`

: Sets the priority of the new process


Other options used for performance tuning:
```elixir
| {:fullsweep_after, non_neg_integer()}
| {:min_heap_size, non_neg_integer()}
| {:min_bin_vheap_size, non_neg_integer()}
```
### Exiting and error handling

#### `exit(pid, reason)`

- Sends an exit signal with the given reason to pid.
- If pid is not trapping exits, pid will exit with given reason.
- If pid is trapping exists, it is be converted to message `{:EXIT, from, reason}`
- If reason is `:normal`, it will only exit if called by `pid` process
- If reason is `:kill`, cannot be trapped and exits with reason `:killed`

#### `flag(flag, value) :: oldvalue`

Set certain flags for the calling process. Among them:

`:trap_exit`  ---  `boolean()`

: Whether process should be trapping exits (see above)

`:error_handler`  ---  `Module`

: Sets given module as the error handler for the process

### Linking

#### `Process.link(pid_or_port)`

- Creates a link between the calling process and the given item
- Linked processes each receive exit signals from the other
- Exit signals other then `:normal` will trigger exit for linked processes
- Can trigger cascading exits, for all transitively linked processes

#### `Process.unlink(pid_or_port)`

- Removes link between the calling process and the given item
- Always returns `true` (ignores all errors)

### Monitoring

#### `monitor(item)`

- Starts monitoring the given item from the calling process.
- Returns a monitoring reference
- When monitored process dies, monitoring process receives

  ```elixir
  {:DOWN, ref, :process, object, reason}
  ```

  + `ref` is the corresponding monitoring reference
  + `object` is the item being monitored
  + `reason` is the exit reason

#### `demonitor(reference(), options :: [:flush | :info])`{.elixir}

Stops monitoring process identified by given monitoring reference

Options:

`:flush`

: removes a `:DOWN` message for the process, if it exists

`:info`

: returns `false` if monitor was not found / could not be removed


### Registering

`register(pid() | port(), atom()) :: true`

: Registers the given pid or port under the name specified as an atom.

`unregister(atom()) :: true`

: Removes the registered name, associated with a pid or a port.

`whereis(atom()) :: pid() | port() | nil`

: Returns the pid or port registered under given name.

```elixir
Process.register(self(), :test)
#=> true
send(:test, :hello)
#=> :hello
Process.unregister(:test)
#=> true
send(:test, :hello)
** (ArgumentError) argument error
```

# Agents

### What is an Agent?

- Simple abstraction around state
- Runs a server maintaining state
- Provides methods to access and update state

. . .

#### State consistency

- access and update is achieved through user-provided functions
- those functions run in the agent
  - to make sure state remains consistent
- Care needed not to execute too much in agent space.

. . .

#### How to use it?

- Define an user API for accessing/updating the intended state
- Implement the API using the methods provided by the agent


### Starting an Agent

Starts a new agent using given function to initialize state.

- `start(fun, options \\ [])`
- `start(module, fun, args, options \\ [])`
- `start_link(fun, options \\ [])`
- `start_link(module, fun, args, options \\ [])`

Upon success returns `{:ok, pid}`.
```elixir
{:ok, pid} = Agent.start_link(fn -> %{} end)
```

#### Among possible options

`{:name, name()}`

: Name to register the process as

`{:spawn_opt, Process.spawn_opt()}`

: Options to be passed to `Process.spawn`


### Updating the state

Updates internal state according to the given function

```elixir
Agent.cast(pid, Map, :put, [:key, :value])
```

#### Synchronously

- Waits (a given timeout) for the update to finish
- if timeout is exceeded, update fails and caller exits

`update(agent, fun, timeout \\ 5000)`

: `fun :: state() -> state()` is the state transformation function

`update(agent, module, fun, args, timeout \\ 5000)`

: As above, but `state` added as first argument to `fun`

#### Asynchronously
`cast(agent, fun)` / `cast(agent, module, fun, args)`

- Same as update, but returns immediately; always succeeds


### Accessing the state

Applies function on internal state and returns result. Synchronous.

- `get(agent(), (state() -> a), timeout()) :: a`
- get(agent, module, fun, args, timeout \\ 5000)
  + As above, but `state` added as first argument to `fun`

```elixir
Agent.get(pid, Map, :get, [:key])
```

#### Get and update

Changes the state and retrieves information simultaneously

- `get_and_update(agent(), (state() -> {a, state()}), timeout()) :: a`
- `get_and_update(agent, module, fun, args, timeout \\ 5000)`


### Storage server as an Agent

```elixir
defmodule Storage do
  use Agent
  def start_link(opts \\ []), do:
    Agent.start_link(fn -> %{} end, opts)

  def get(storage, key), do:
    Agent.get(storage, Map, :get, [key])

  def put(storage, key, value), do:
    Agent.get_and_update(storage, __MODULE__, 
        :put_handler, [key, value])
  def put_handler(state, key, value) do
    case Map.get(state, key) do
      nil -> {:ok, Map.put(state, key, value)}
      _value -> {:not_ok, state}
    end
  end
end
```

# `GenServer`: Servers and callbacks

### `GenServer` behaviour

- abstracts the common client-server interaction
- only need to implement callbacks and client API
- Provides functions for triggering callbacks on server

#### Callbacks

- Run on the server and can access / update state
- Handle messages/events
- Synchronous (return value) / Asynchronous
- Identify client

#### Client API

- Runs on the client invoking the API
- Uses `Genserver` functions for triggering needed callbacks


## Callbacks

###  `init/1` --- process initialization callback

- Uses its argument to initialize the state of the server
- Will be called by `start` / `start_link` after starting the process

#### Exit values

`{:ok, state}`

: Success, with the new internal state

`{:ok, state, continue}`

: Success, but `continue` might be
  - a timeout to sleep before waiting for input
  - `:hibernate` to hibernate before waiting for input
  - `{:continue, how}` to trigger post-initialization callback

`:ignore`

: Failure, with indication that it should be ignored

`{:stop, reason}`

: Failure which should be propagated (triggers exit)
: Should do any cleanup required.

### `terminate(reason, state)` --- process cleanup callback

- Invoked when the server is about to exit
- Should do any cleanup required

#### When it is called?

If a callback (except init/1) does one of the following:

- returns a `:stop` tuple
- raises an exception
- calls Kernel.exit/1
- returns an invalid value
- the GenServer traps exits (using Process.flag/2) and the parent process sends an exit signal


### `handle_cast(request, state)` --- asynchronous callback

Used to handle `cast` messages

- `state` update based on `request` message
- does not reply; Client does not wait for callback to finish (non-blocking)

#### Exit values

```elixir
{:noreply, new_state}
{:noreply, new_state, continue}
```
- Success, with the new internal state
- Might `continue` with timeout / hybernate / callback

```elixir
{:stop, reason, new_state}
```

Failure for a `reason`.

- Will invoke `terminate` on the new_state
- Will trigger `exit` with `reason`

### `handle_call(request, from, state)`

Used to handle `call` messages

- Client (`from`) waits for an answer / reply (blocking)
- `state` update based on `request` message

#### Exit values

```elixir
{:reply, reply, new_state} / {:reply, reply, new_state, continue}
```
- Sends `reply` to the Client

```elixir
{:noreply, new_state} / {:noreply, new_state, continue}
```
- Does not send a reply to the Client
- Reply __must__ be sent using `reply/2`
  + Client is blocked waiting for the reply 

```elixir
{:stop, reason, new_state} / {:stop, reason, reply, new_state}
```

- Invokes `terminate` on the new_state
- Will then send `reply` to the Client, if specified
- Triggers `exit` with `reason`

### `handle_info` and `handle_continue`

#### `handle_info(msg, state)`

- Handles messages not meant for callbacks
- Exit values similar to handle_cast

#### `handle_continue(continue, state)`

- Handles messages generated through `:continue` returns
- Exit values similar to handle_cast

## Example: IRC Channel

### Channel --- Client API 

Clear, functionality implemented as functions:

```elixir
defmodule Channel do
  use GenServer

  def start_link(channel_name, opts \\ []), do:
    GenServer.start_link(__MODULE__, channel_name, opts)

  def post(server, message), do:
    GenServer.cast(server, {:post, message})

  def join(server, name, address), do:
    GenServer.cast(server, {:join, name, address})

  def leave(server, name), do:
    GenServer.cast(server, {:leave, name})
```

### Channel --- init callback

```elixir
  @impl true
  def init(channel_name) do
    {:ok, userAgent} = Storage.start_link()
    {:ok, %ChannelState{
      users: userAgent,
      name: channel_name,
    }}
  end
```

- Starts a new Storage unit to remember users in channel
- Initializes the Server with the `%ChannelState`

### Channel --- `post` callback

```elixir
  @impl true
  def handle_cast({:post, message}, state) do
    Storage.foreach(state.users,
        &send(&1, {:post, state.name, message}))
    {:noreply, state}
  end
```

- Broadcasts message to all users in channel
- Does not change state

### Channel --- `join` callback

```elixir
  @impl true
  def handle_cast({:join, name, address}, state) do
    case Storage.put(state.users, name, address) do
      :ok -> Channel.post(self(), "#{name} has joined the channel")
      :not_ok -> IO.puts :stderr, "#{name} already in the channel"
    end
    {:noreply, state}
  end
```

Attempts to add user to channel

- If successful, posts entry message to itself
- If user already in channel, logs error to `:stderr`
- Does not change the state

### Channel --- `delete` callback

```elixir
  @impl true
  def handle_cast({:leave, name}, state) do
    Storage.delete(state.users, name)
    Channel.post(self(), "#{name} has left the channel")
    {:noreply, state}
  end
```

- Removes user from channel
- Posts exist message to itself
- Does not change the state


# Supervisors

### What is a Supervisor

A Supervisor is a process _supervising_ other processes

- Provides fault-tolerance
- Encapsulates application start-up / shut-down

#### Supervisor duties

- Start the child processes
- Restart them upon failure or certain condition reached
- Orderly terminate them upon system shutdown

### Example: Supervising the server

```elixir
defmodule ServerSupervisor do
  use Supervisor

  def start_link(opts \\ []), do:
    Supervisor.start_link(__MODULE__, :ok, opts)

  @impl true
  def init(:ok) do
    children = [
      {Server, name: :server}
    ]
    Supervisor.init(children, strategy: :one_for_one)
  end
end
```

### Example: Supervising the server test
```elixir
  test "supervisor" do
    {:ok, supervisor} = ServerSupervisor.start_link()
    Server.register(:server, :user)
    assert_receive {:post, :general, "user has joined the channel"}
    pid = Process.whereis(:server)
    Process.exit(pid, :kill)
    Process.sleep(1)
    Server.register(:server, :user)
    assert_receive {:post, :general, "user has joined the channel"}
  end
```

### Init options

#### `strategy`

Describes strategy used for restarting

`one_for_one`

: restarts only the failing process

`all_for_one`

: restarts all processes when one fails

`rest_for_one`

: restarts failing process and processes started after it

#### `max_restarts`

Number of restarts allowed in a time frame; default 3

#### `max_seconds`

Time frame in which `:max_restarts` applies; default 5

#### `name`

- A register name for the supervisor process; optional

### Child specifications

`spawn_link/2` or `init/2` receives a list of child specifications

- Either the name of a module to invoke `start_link` on
- Or a tuple of a `module` and options to be passed to `start_link`
- Or a full child specification
  ```elixir
  child_spec() :: %{
    :id => atom() | term(),
    :start => {module(), atom(), [term()]},
    optional(:restart) => :permanent | :transient | :temporary,
    optional(:shutdown) => timeout() | :brutal_kill,
    optional(:type) => :worker | :supervisor,
    optional(:modules) => [module()] | :dynamic
  }
  ```

  - Can be specified as extra arguments to `use GenServer`