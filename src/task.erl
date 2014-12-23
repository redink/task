-module(task).

-export([async/3,
         await/1,
         await/2]).

-export([async_do/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-export([wait_and_send/2]).
-endif.

async(Mod, Fun, Args) ->
    Me  = erlang:self(),
    Pid = proc_lib:spawn_link(?MODULE, async_do,
                              [Me, get_info(Me), {Mod, Fun, Args}]),
    Ref = erlang:monitor(process, Pid),
    erlang:send(Pid, {Me, Ref}),
    {Pid, Ref}.

await({Pid, Ref}) ->
    await({Pid, Ref}, 5000).

await({Pid, Ref}, TimeOut) ->
    receive
        {Ref, Reply} ->
            erlang:demonitor(Ref, [flush]),
            Reply;
        {'DOWN', Ref, _, _, noconnection} ->
            erlang:exit({nodedown, erlang:node(Pid),
                         {?MODULE, await, [{Pid, Ref}, TimeOut]}});
        {'DOWN', Ref, _, _, Reason} ->
            erlang:exit({Reason,
                         {?MODULE, await, [{Pid, Ref}, TimeOut]}})
    after TimeOut ->
        erlang:demonitor(Ref, [flush]),
        erlang:exit({timeout,
                    {?MODULE, await, [{Pid, Ref}, TimeOut]}})
    end.

async_do(TaskOwner, TaskOwnerInfo, MFA) ->
    initial_call(MFA),
    Ref =
        receive
            {TaskOwner, Ref1} ->
                Ref1
        end,
    erlang:send(TaskOwner, {Ref, do_apply(TaskOwnerInfo, MFA)}).

get_info(Pid) ->
    Name =
        case erlang:process_info(Pid, [registered_name]) of
            [{registered_name, []}] ->
                Pid;
            [{registered_name, RegisteredName}] ->
                RegisteredName
        end,
    {erlang:node(), Name}.

initial_call(MFA) ->
    erlang:put('$initial_call', get_initial_call(MFA)).

get_initial_call({Mod, Fun, Args}) ->
    {Mod, Fun, erlang:length(Args)}.

do_apply(TaskOwnerInfo, {Mod, Fun, Args} = MFA) ->
    try
        erlang:apply(Mod, Fun, Args)
    catch
        error: Value ->
            task_exit(TaskOwnerInfo, MFA, {Value, erlang:get_stacktrace()});
        throw: Value ->
            task_exit(TaskOwnerInfo, MFA, {{nocatch, Value}, erlang:get_stacktrace()});
        exit: Value ->
            task_exit(TaskOwnerInfo, MFA, Value)
    end.

task_exit(_, _, normal) ->
    erlang:exit(normal);
task_exit(_, _, shutdown) ->
    erlang:exit(shutdown);
task_exit(_, _, Reason) when erlang:tuple_size(Reason) =:=2 
                     andalso erlang:element(2, Reason) =:= shutdown ->
    erlang:exit(Reason);
task_exit(TaskOwnerInfo, MFA, Reason) ->
    {Fun, Args} = get_running(MFA),

    error_logger:format(
        "** Task ~p terminating~n" ++
        "** Started from ~p~n" ++
        "** When function == ~p~n" ++
        "**      arguments == ~p~n" ++
        "** Reason for termination == ~n" ++
        "** ~p~n", [erlang:self(), get_from(TaskOwnerInfo), Fun, Args, Reason]),
    erlang:exit(Reason).

get_from({Node, PidOrName}) when Node =:= erlang:node() ->
    PidOrName;
get_from(Other) ->
    Other.

get_running({Mod, Fun, Args}) ->
    {erlang:make_fun(Mod, Fun, erlang:length(Args)), Args}.

-ifdef(TEST).

wait_and_send(TaskOwner, Atom) ->
    erlang:send(TaskOwner, ready),
    receive
        true ->
            true
    end,
    erlang:send(TaskOwner, Atom).

%% async/3
task_async_test_() ->
    {Pid, _} = Task = task:async(task, wait_and_send, [self(), done]),
    receive
        ready ->
            ok
    end,

    erlang:send(Pid, true),

    [?_assertEqual(true, lists:member(Pid, erlang:element(2, erlang:process_info(self(), links)))),     
     ?_assert(task:await(Task) =:= done)].

%% await/1 exits on timeout
task_await_1_test_() ->
    Task = {undefined, erlang:make_ref()},
    ?_assertEqual(catch task:await(Task, 0), {'EXIT', {timeout, {task, await, [Task, 0]}}}).

%% await/1 exits on normal exit
task_await_2_test_() ->
    Task = task:async(erlang, exit, [normal]),
    ?_assertEqual(catch task:await(Task), {'EXIT', {normal, {task, await, [Task, 5000]}}}).

%% await/1 exits on task throw
task_await_3_test_() ->
    erlang:process_flag(trap_exit, true),
    Task = task:async(erlang, throw, [unknown]),
    ?_assertMatch({'EXIT',{{{nocatch,unknown},_},{task,await,[Task, 5000]}}}, catch task:await(Task)).

%% await/1 exits on task exit
task_await_4_test_() ->
    erlang:process_flag(trap_exit, true),
    Task = task:async(erlang, exit, [unknown]),
    ?_assertEqual(catch task:await(Task), {'EXIT', {unknown, {task, await, [Task, 5000]}}}).

%% await/1 exits on :noconnection
task_await_5_test_() ->
    Ref  = erlang:make_ref(),
    Pid  = erlang:self(),
    Task = {Pid, Ref},

    erlang:send(Pid, {'DOWN', Ref, Pid, Pid, noconnection}),

    ?_assertEqual(catch task:await(Task), {'EXIT',{nodedown,nonode@nohost,{task,await,[Task,5000]}}}).


    
-endif.
