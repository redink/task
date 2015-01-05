-module(task_test).

-include_lib("eunit/include/eunit.hrl").

-export([wait_and_send/2]).

wait_and_send(TaskOwner, Atom) ->
    erlang:send(TaskOwner, ready),
    receive
        true ->
            true
    end,
    erlang:send(TaskOwner, Atom).

task_test_() ->
    {setup,
     fun() ->
             ok
     end,
     fun(_) ->
             [
              {"async/3",
               fun() ->
                       {Pid, _} = Task = task:async(?MODULE, wait_and_send, [self(), done]),

                       receive
                           ready ->
                               ok
                       end,

                       erlang:send(Pid, true),

                       ?assertEqual(true, lists:member(Pid, erlang:element(2, erlang:process_info(self(), links)))),
                       ?assertEqual(done, task:await(Task))
               end},
              {"async/4",
               fun() ->
                       net_kernel:start(['test@127.0.0.1']),
                       {Pid, _} = Task = task:async('test@127.0.0.1', ?MODULE, wait_and_send, [self(), done]),

                       ?assertEqual('test@127.0.0.1', erlang:node(Pid)),
                       receive
                           ready ->
                               ok
                       end,

                       erlang:send(Pid, true),
                       ?assertEqual(true, lists:member(Pid, erlang:element(2, erlang:process_info(self(), links)))),
                       ?assertEqual(done, task:await(Task))
               end},
              {"async_opt/4",
               fun() ->
                       Opts = [{fullsweep_after, 10}, {priority, high}],
                       {Pid, _} = Task = task:async_opt(?MODULE, wait_and_send, [self(), done], Opts),
                       ?assertEqual({priority, high}, erlang:process_info(Pid, priority)),
                       ?assertEqual(10, proplists:get_value(fullsweep_after, erlang:element(2, erlang:process_info(Pid, garbage_collection)))),

                       receive
                           ready ->
                               ok
                       end,

                       erlang:send(Pid, true),
                       ?assertEqual(true, lists:member(Pid, erlang:element(2, erlang:process_info(self(), links)))),
                       ?assertEqual(done, task:await(Task))
               end},
              {"async_opt/5",
               fun() ->
                       net_kernel:start(['test@127.0.0.1']),
                       Opts = [{fullsweep_after, 10}, {priority, high}],
                       {Pid, _} = Task = task:async_opt('test@127.0.0.1', ?MODULE, wait_and_send, [self(), done], Opts),

                       ?assertEqual('test@127.0.0.1', erlang:node(Pid)),
                       ?assertEqual({priority, high}, erlang:process_info(Pid, priority)),
                       ?assertEqual(10, proplists:get_value(fullsweep_after, erlang:element(2, erlang:process_info(Pid, garbage_collection)))),

                       receive
                           ready ->
                               ok
                       end,

                       erlang:send(Pid, true),
                       ?assertEqual(true, lists:member(Pid, erlang:element(2, erlang:process_info(self(), links)))),
                       ?assertEqual(done, task:await(Task))
               end},
              {"await/1 exits on timeout",
               fun() ->
                       Task = {undefined, erlang:make_ref()},
                       ?_assertEqual(catch task:await(Task, 0), {'EXIT', {timeout, {task, await, [Task, 0]}}})
               end},
              {"await/1 exits on normal exit",
               fun() ->
                       Task = task:async(erlang, exit, [normal]),
                       ?_assertEqual(catch task:await(Task), {'EXIT', {normal, {task, await, [Task, 5000]}}})
               end},
              {"await/1 exits on task throw",
               fun() ->
                       erlang:process_flag(trap_exit, true),
                       Task = task:async(erlang, throw, [unknown]),
                       ?_assertMatch({'EXIT',{{{nocatch,unknown},_},{task,await,[Task, 5000]}}}, catch task:await(Task))
               end},
              {"await/1 exits on task exit",
               fun() ->
                       erlang:process_flag(trap_exit, true),
                       Task = task:async(erlang, exit, [unknown]),
                       ?_assertEqual(catch task:await(Task), {'EXIT', {unknown, {task, await, [Task, 5000]}}})
               end},
              {"await/1 exits on noconnection",
               fun() ->
                       Ref  = erlang:make_ref(),
                       Pid  = erlang:self(),
                       Task = {Pid, Ref},

                       erlang:send(Pid, {'DOWN', Ref, Pid, Pid, noconnection}),

                       ?_assertEqual(catch task:await(Task), {'EXIT',{nodedown,nonode@nohost,{task,await,[Task,5000]}}})
               end}
             ]
     end
    }.
