defmodule ServerState do
  @enforce_keys [:users, :channels, :userThreads]
  defstruct users: nil, channels: nil, userThreads: nil
  @type t :: %__MODULE__{
    users: Process.dest(),
    channels: Process.dest(),
    userThreads: Process.dest(),
  }
end
