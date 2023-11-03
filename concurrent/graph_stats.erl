-module(graph_stats).
-export([start/0, start/3]).

-record(partition_input, {
			  id = nil, 
			  node_ids = nil, 
			  colors = nil, 
			  edges = nil}).
-record(node, {id, color, edges = []}).

%% - Count the number of nodes of each color in the local partition gp.
%% - For each color c in the local partition, 
%%   sum the degree of the nodes of that color 
%%   (where the degree of a node is equal to the number of edges it belongs to).
%% - Return the node count and degree of each color in the local partition.

start() ->
    start("../input.txt", "../result_a.txt", "../result_b.txt").

start(InputPath, ResultAPath, ResultBPath) -> 
    graph_supervisor:start_link(ResultAPath, ResultBPath),
    process_input(InputPath),
    ok.
	    
process_input(Path) ->
    {ok, File} = file:open(Path, read),
    read_line(File, #partition_input{}),
    file:close(File).

read_line(File, #partition_input{id = nil} = PartitionInput) ->
    {ok, Line} = file:read_line(File),
    Id = string:trim(Line),
    read_line(File, PartitionInput#partition_input{id = Id});

read_line(File, #partition_input{node_ids = nil} = PartitionInput) ->
    {ok, Line} = file:read_line(File),
    NodeIds = string:split(string:trim(Line), ",",  all), 
    read_line(File, PartitionInput#partition_input{node_ids = NodeIds});

read_line(File, #partition_input{colors = nil} = PartitionInput) ->
    {ok, Line} = file:read_line(File),
    Colors = string:split(string:trim(Line), ",", all),
    read_line(File, PartitionInput#partition_input{colors = Colors});

read_line(File, #partition_input{node_ids = NodeIds, edges = nil} = PartitionInput) ->
    SplitPairString = fun(Pair) -> 
        [K, V] = string:split(Pair, ","), 
	{K, V} 
    end,
    ReversePairs = fun({X,Y}) -> {Y, X} end,
    GroupByKey = fun({K, _V}) -> K end,
    GroupByVal = fun({_K, V}) -> V end,
    {ok, Line} = file:read_line(File),
    Pairs = lists:map(SplitPairString, string:split(string:trim(Line), " ", all)),
    ReversedPairs = lists:map(ReversePairs, Pairs),
    AllPairs = lists:uniq(Pairs ++ ReversedPairs),
    GroupedPairs = maps:groups_from_list(GroupByKey, GroupByVal, AllPairs),
    GetNodeEdges = fun(K) -> {K, maps:get(K, GroupedPairs, [])} end,
    Edges = lists:map(GetNodeEdges, NodeIds),
    read_line(File, PartitionInput#partition_input{edges = Edges});

read_line(File, #partition_input{id = PartitionId} = PartitionInput) ->
    case file:read_line(File) of
	eof ->
	    %% CALL RESULT REPORTER HERE
	    Nodes = parse_nodes(PartitionInput),
	    graph_supervisor:add_partition(PartitionId, Nodes),
	    ok;
	{ok, Line} ->
	    Nodes = parse_nodes(PartitionInput),
	    graph_supervisor:add_partition(PartitionId, Nodes),
	    Id = string:trim(Line),
	    read_line(File, #partition_input{id = Id})
    end.

parse_nodes(#partition_input{
	       node_ids = NodeIds, 
	       colors = Colors, 
	       edges = Edges}) ->
    BuildNode = fun({NodeId, Edges_}, {NodeId, Color} ) ->
        #node{id = NodeId, color = Color, edges = Edges_}
    end,
    NodeColors = lists:zip(NodeIds, Colors),
    lists:zipwith(BuildNode, Edges, NodeColors).





			  

    
    
    
