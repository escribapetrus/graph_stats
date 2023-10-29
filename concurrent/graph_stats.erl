-module(graph_stats).
-export([start/0, start/3, partition/2, write_results/4]).

-record(partition_input, {id = nil, node_ids = nil, colors = nil, edges = nil}).
-record(node, {id, color, edges = []}).


%% - Count the number of nodes of each color in the local partition gp.
%% - For each color c in the local partition, 
%%   sum the degree of the nodes of that color 
%%   (where the degree of a node is equal to the number of edges it belongs to).
%% - Return the node count and degree of each color in the local partition.

start() ->
    start("../input.txt", "../result_a.txt", "result_b.txt").
    

start(InputPath, _ResultAPath, _ResultBPath) -> 
    process_input(InputPath, nil),
    ok.

write_results(_, _, _ResultAFilePath, _ResultBFilePath) ->
    ok.
    
%%%%%%%%%%%%%%%%%%%%%%%%%
%% PARTITION WORKER
%% State: PartitionId, #{NodeId => {Color, [Edges]}}


partition(PartitionId, Nodes) ->
    erlang:display({PartitionId, Nodes}).
	    
	    
%%%%%%%%%%%%%%%%%%%%%%%
%% FILE PROCESSING

process_input(Path, ResultReporterPid) ->
    {ok, File} = file:open(Path, read),
    read_line(File, ResultReporterPid, #partition_input{}).

read_line(File, ResultReporterPid, #partition_input{id = nil} = PartitionInput) ->
    {ok, Line} = file:read_line(File),
    Id = string:trim(Line),
    read_line(File, ResultReporterPid, PartitionInput#partition_input{id = Id});

read_line(File, ResultReporterPid, #partition_input{node_ids = nil} = PartitionInput) ->
    {ok, Line} = file:read_line(File),
    NodeIds = string:split(string:trim(Line), ",",  all), 
    read_line(File, ResultReporterPid, PartitionInput#partition_input{node_ids = NodeIds});

read_line(File, ResultReporterPid, #partition_input{colors = nil} = PartitionInput) ->
    {ok, Line} = file:read_line(File),
    Colors = string:split(string:trim(Line), ",", all),
    read_line(File, ResultReporterPid, PartitionInput#partition_input{colors = Colors});

read_line(File, ResultReporterPid, #partition_input{node_ids = NodeIds, edges = nil} = PartitionInput) ->
    {ok, Line} = file:read_line(File),
    Pairs = string:split(string:trim(Line), " ", all),
    GroupedPairs = maps:groups_from_list(fun(Pair) ->
						 [NodeId, _Edge] = string:split(Pair, ","),
						 NodeId end, 
					 fun(Pair) ->
						 [_NodeId, Edge] = string:split(Pair, ","),
						 Edge end, 
					 Pairs),

    Edges = lists:map(fun(NodeId) -> {NodeId, maps:get(NodeId, GroupedPairs, [])} end, NodeIds),
    erlang:display({"PARTITION INPUT", Edges}),
    read_line(File, ResultReporterPid, PartitionInput#partition_input{edges = Edges});

read_line(File, ResultReporterPid, #partition_input{id = PartitionId} = PartitionInput) ->
    case file:read_line(File) of
	eof ->
	    %% CALL RESULT REPORTER HERE
	    ok;
	{ok, Line} ->
	    Nodes = parse_nodes(PartitionInput),
	    spawn(graph_stats, partition, [PartitionId, Nodes]),
	    Id = string:trim(Line),
	    read_line(File, ResultReporterPid, #partition_input{id = Id})
    end.

parse_nodes(#partition_input{node_ids = NodeIds, colors = Colors, edges = Edges} = _PartitionInput) ->
    NodeColors = lists:zip(NodeIds, Colors),
    lists:zipwith(fun({NodeId, Color}, {NodeId, Edges_}) ->
				      #node{id = NodeId, color = Color, edges = Edges_}
			      end,
			      Edges,
			      NodeColors).
   




			  

    
    
    
