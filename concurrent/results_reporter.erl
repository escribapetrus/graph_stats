-module(results_reporter).
-export([start_link/2, 
         add_color_count_and_degree/3, add_most_influential_nodes/2,
	 write_color_count_and_degrees/0, write_most_influential_nodes/0]).
-export([init/1, handle_cast/2, handle_call/3]).
-export([test/0]).
-behaviour(gen_server).

-record(state, {color_count_and_degrees = maps:new(),
		most_influential_nodes = maps:new(),
		result_a_path, 
		result_b_path}).

start_link(ResultAPath, ResultBPath) ->
    InitialState = #state{result_a_path = ResultAPath, result_b_path = ResultBPath},
    gen_server:start_link({local, results_reporter}, results_reporter, InitialState, []).

add_color_count_and_degree(Color, Nodes, Edges) ->
    gen_server:cast(results_reporter, {add_color_count_and_degree, {Color, Nodes, Edges}}).

add_most_influential_nodes(PartitionId, MostInfluential) ->
    gen_server:cast(results_reporter, {add_most_influential_nodes, {PartitionId, MostInfluential}}).

write_color_count_and_degrees() ->
    gen_server:cast(results_reporter, write_color_count_and_degrees).

write_most_influential_nodes() ->
    gen_server:cast(results_reporter, write_most_influential_nodes).

%% CALLBACKS
init(#state{} = State) ->
    {ok, State}.

handle_cast({add_color_count_and_degree, {Color, NewCount, NewDegree}}, 
	    #state{color_count_and_degrees = ColorCountAndDegrees} = State) ->
    Update = fun({Count, Degree}) -> {NewCount + Count, NewDegree + Degree} end,
    UpdatedColorCountAndDegrees = maps:update_with(Color, Update, {NewCount, NewDegree}, ColorCountAndDegrees),
    {noreply, State#state{color_count_and_degrees = UpdatedColorCountAndDegrees}};

handle_cast(write_color_count_and_degrees, 
            #state{
               color_count_and_degrees = ColorCountAndDegrees, 
               result_a_path = ResultAPath} = State) -> 
    {ok, File} = file:open(ResultAPath, [append]),
    WriteLine = fun(Color, {Count, Degree}) -> 
                        io:format(File, "~s: ~p ~p~n", [Color, Count, Degree])
               end, 
    maps:foreach(WriteLine, ColorCountAndDegrees),
    file:close(File),
    {noreply, State};

handle_cast({add_result_b, 
             {PartitionId, MostInfluentialNodes}}, 
            #state{most_influential_nodes = MostInfluentialNodes} = State) ->
    Update = fun(M) -> M ++ MostInfluentialNodes end,
    UpdatedMostInfluentialNodes = maps:update_with(PartitionId,
					      Update,
					      MostInfluentialNodes),
    {noreply, State#state{most_influential_nodes = UpdatedMostInfluentialNodes}}.

handle_call(get_state, _From, State) ->		 
    {reply, State, State}.

%%%%%%%%%%%%%%%%% TESTS

test() ->
    TestData = lists:flatten([
     [{"red",1,3},{"green",1,4},{"blue",2,7}],
     [{"red",1,4},{"green",3,10}],
     [{"red",2,7},{"blue",2,7}]
    ]),
    {ok, _Pid} = results_reporter:start_link("../results_a_test.txt", "../results_b_test.txt"),

    lists:foreach(fun({Color, Count, Degree}) -> 
                             results_reporter:add_color_count_and_degree(Color, Count, Degree)
                  end, TestData),
    
    results_reporter:write_color_count_and_degrees().

