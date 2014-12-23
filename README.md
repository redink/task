# task
`Task` is an [Elixir Task](http://elixir-lang.org/getting_started/mix_otp/8.html#8.2-tasks) model partial implement in Erlang.

Detailed doc, [here](https://github.com/elixir-lang/elixir/blob/v1.0.1/lib/elixir/lib/task.ex).

## async and await

In this repo, the way to spawn a task is with `task:async/3`. Then, a new process will be created, linked and monitored by the caller(that is the task owner). Once the task action finishes, a message will be sent to the caller with the result.

`task:await/1` and `task:await/2` is used to read the message sent bt the task process. On `await`, `Task` will also setup a monitor to verify if the process exited for any abnormal reason(or in case exits are being trapped by the caller). 