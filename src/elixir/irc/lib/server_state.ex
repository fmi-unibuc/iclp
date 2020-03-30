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
