%%%%%%%%%%%%%%%%%%%%%%%%%
%% RESULTS REPORTER WORKER

-module(results_reporter).
- export([start_link/0, add_result_a/3]).
-export([init/1, handle_cast/2, handle_call/3]).
-behaviour(gen_server).

-record(state, {result_a, result_b, partition_count, result_count}).

%% API

start_link() ->
    InitialState = #state{},
    gen_server:start_link({local, results_reporter}, results_reporter, InitialState, []).


add_result_a(Color, NewCount, NewDegree) ->
    gen_server:handle_cast({Color, NewCount, NewDegree}).


%% CALLBACKS

init(#state{} = State) ->
    {ok, State}.

handle_cast({add_result_a, {Color, NewCount, NewDegree}}, #state{result_a = ResultA} = State) ->
    UpdatedResultA = maps:update_with(Color, 
				      fun({Count, Degree}) -> 
					      {Count + NewCount, Degree + NewDegree} end,
				      ResultA),
    {noreply, State#state{result_a = UpdatedResultA}};

handle_cast({add_result_b, {PartitionId, MostInfluential}}, #state{result_b = ResultB} = State) ->
    UpdatedMostInfluential = maps:update_with(PartitionId,
					      fun(M) -> M ++ MostInfluential end,
					      ResultB),
    {noreply, State#state{result_b = UpdatedMostInfluential}}.

handle_call(get_state, _From, State) ->		 
    {reply, State, State}.

