%%%%%%%%%%%%%%%%%%%%%%%%%
%% RESULTS REPORTER WORKER

-module(results_reporter).
-export([start_link/0, start_link/2, add_result_a/3, add_result_b/2,
	 write_result_a/0]).
-export([init/1, handle_cast/2, handle_call/3]).
-behaviour(gen_server).

-record(state, {result_a = maps:new(),
		result_b = maps:new(),
		result_a_path, 
		result_b_path}).

%% API
start_link() ->
    start_link(nil, nil).

start_link(ResultAPath, ResultBPath) ->
    InitialState = #state{result_a_path = ResultAPath, result_b_path = ResultBPath},
    gen_server:start_link({local, results_reporter}, results_reporter, InitialState, []).


add_result_a(Color, Nodes, Edges) ->
    gen_server:cast(results_reporter, {add_result_a, {Color, Nodes, Edges}}).

add_result_b(PartitionId, MostInfluential) ->
    gen_server:cast(results_reporter, {PartitionId, MostInfluential}).

write_result_a() ->
    gen_server:cast(results_reporter, write_result_a).

%% CALLBACKS
init(#state{} = State) ->
    {ok, State}.

handle_cast({add_result_a, {Color, NewNodes, NewEdges}}, 
	    #state{result_a = ResultA} = State) ->
    UpdatedResultA = maps:update_with(Color, 
				      fun({Nodes, Edges}) -> 
					      {
					       NewNodes ++ Nodes, 
					       NewEdges ++ Edges
					      } 
				      end,
				      {[], []},
				      ResultA),
    {noreply, State#state{result_a = UpdatedResultA}};

handle_cast(write_result_a, #state{result_a = ResultA, result_a_path = ResultAPath} = State) -> 
    {ok, File} = file:open(ResultAPath, append),
    maps:foreach(fun(Color, {Nodes, Edges}) -> 
			 Line = Color ++ ", " ++ length(Nodes) ++ length(lists:uniq(Edges)),
			 file:write(File, list_to_binary(Line))
		 end, 
		 ResultA),
    file:close(File),
	{noreply, State};

handle_cast({add_result_b, {PartitionId, MostInfluential}}, #state{result_b = ResultB} = State) ->
    UpdatedMostInfluential = maps:update_with(PartitionId,
					      fun(M) -> M ++ MostInfluential end,
					      ResultB),
    {noreply, State#state{result_b = UpdatedMostInfluential}}.

handle_call(get_state, _From, State) ->		 
    {reply, State, State}.

