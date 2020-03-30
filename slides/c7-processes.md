---
title: Implementarea Concurenței în limbaje de programare
subtitle: Actor-based programming --- messages and processes
author: Traian Florin Șerbănuță
institute: FMI @ UNIBUC
abstract: |
---

# Processes and messages

## Remember Actors?

An actor is a computational entity that, in response to a message it receives, can concurrently:

- send a messages to other actors;
- create new actors;
- designate the behavior to be used for the next message it receives.

## Sending and receiving messages

### 
```elixir
send(dest :: Process.dest(), message) :: message
  when message: any()
```

- Puts a `message` into `dest`'s mailbox (non-blocking)

### `receive`{.elixir}
- Matches mailbox messages against supplied patterns, in order
- First message matching a pattern is consumed
  + corresponding action is executed with the obtained bindings
- If no message matches any pattern, waits for new messages (blocking)
- Timeout can be specified using `after`{.elixir} clause

###
```elixir
me = self()
send me, "Hello!"
receive do
  message -> IO.puts "Received: " <> message
end
```

## Creating processes

### `spawn(module(), atom(), list()) :: pid()`{.elixir}
- Arguments
  - the name (alias) of a module
  - an atom representing the name of a function in that module
  - a list of arguments to be passed to that function
- Spawns a new process executing the function with the given arguments
- Returns a process identifier
  - useful as an address to communicate with the process

```elixir
    server = spawn(Pong, :init, [])
    me = self()

    send server, {:ping, me}
    receive do
      :pong -> IO.puts "received pong"
    end
```

## Message Loop

```elixir
defmodule Pong do
  def init() do
    receive do
      {:ping, from} -> send from, :pong
    end
    init()
  end
end
```

### Server Pattern
- After (potentially) some initial setup
- Call a recursive function which
  - Uses a `receive`{.elixir} statement to handle possible messages
  - Recursively calls the function to continue handling messages

## Server with (Evolving) Internal State

- Standard way to maintain state in a functional language
  - State passed along as additional argument(s) to loop function
  - State updated when recalling the loop function
- Designates the behavior to be used the next message

```elixir
defmodule Stack do
  def init(), do: loop([])
  defp loop(state) do
    receive do
      {:push, value} -> loop([value | state])
      {:pop, reply} ->
        case state do
          [h | t] -> send(reply, {:some, h}) ;  loop(t)
          []      -> send(reply, {:none})    ;  loop([])
        end
    end
  end
end
```

## Continuations: Sending Out Code

- Message can carry anything, including closures (functions)
- Those functions can be executes by the message handlers

```elixir
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
stack = spawn(&Stack.init/0)
send(stack, {:push, 10})
send(stack, {:pop, &IO.puts/1})
```


# Case Study: a chat server architecture

## The server

- Maintains a list of users
  + users can join/leave the server
  + nicknames and addresses

- Maintains a list of channels (chat rooms)
  + public channels (available for all)
  + private channels (one-on-one, or several parties)
  + Allows users to create channels

- Broadcasts disconnection information to all channels

## The channels

### All channels
- Maintain the list of users active within the channel
  + A subset of the users registered to the server
  + users can join/leave the channel

- Distribute messages posted to the channel to all users

### The `#general` channel
- Evaryting above, and something more
- Channel `#general` is automatically joined by all users
- Leaving `#general` automatically disconects a user


## The users

### Interaction with the server

- Can join/leave the server
- Can request the list of channels
- Can create channels (public/private)

### Interaction with a channel

- Can join/leave a channel
- Can request the list of users in the channel
- Receive all messages posted in that channel

# Implementation details

## A Storage service

Functionality

: inserting, updating and querying a key-value map

### Interface (messages)

`{:put, key, value, action}`{.elixir}

: adds an entry if `key` not present. Runs `action` on success/error message

`{:put!, key, value}`{.elixir}

: adds/updates an entry

`{:get, key, action}`{.elixir}

: retrieves the value for `key` and consumes it using `action`

`{:delete, key}`{.elixir}

: removes a value from the map

`{:foreach, action}`{.elixir}

: applies `action` to all values in the map


## Storage service implementation

```elixir
defmodule Storage do
  def init(state \\ %{}) do
    receive do
      {:put, key, value, action} ->
        case Map.get(state, key) do
          nil    -> action.(:ok) ; init(Map.put(state, key, value))
          _value -> action.(:not_ok)                  ; init(state)
        end
      {:put!, key, value} ->       init(Map.put(state, key, value))
      {:get, key, action} ->
        action.(Map.get(state, key))                  ; init(state)
      {:delete, key} ->                init(Map.delete(state, key))
      {:foreach, action} ->
        Enum.each(Map.values(state), action)          ; init(state)
    end
  end
end

```



## Channel State
```elixir
defmodule ChannelState do
  @enforce_keys [:name, :users]
  defstruct name: nil, users: nil
  @type t :: %__MODULE__{
    name: atom(),
    users: Process.dest()
  }
end
```

`name`

: the (public) name of the channel

`users`

: channel's users registry process

## Server State

```elixir
defmodule ServerState do
  @enforce_keys [:me, :users, :channels, :userThreads]
  defstruct me: nil, users: nil, channels: nil, userThreads: nil
  @type t :: %__MODULE__{
    me: Process.dest(),
    users: Process.dest(),
    channels: Process.dest(),
    userThreads: Process.dest(),
  }
end
```

`me`

: the address of the server

`users`

: server's users registry process

`channels`

: server's channels registry process

`userThreads`

: server's user threads registry process

## Server Threads State
```elixir
defmodule ServerThreadState do
  defstruct
    userName: nil, userAddress: nil,
    userChannels: nil, globalChannels: nil
end
```

`userName`

: (public) name of the user

`userAddress`

: (private) address of the user

`userChannels`

: user's joined channels registry process

`globalChannels`

: server's channels registry process

