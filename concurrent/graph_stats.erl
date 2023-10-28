-module(graph_stats).
-export([start/0, start/3]).
-behaviour(gen_server).

-record(node, {id, color, edges = []}).
-record(result_a, {color, count, degree}.
-record(result_b, {partition_id, most_influential_nodes}).
-record(worker, {nodes, result}

%% - Count the number of nodes of each color in the local partition gp.
%% - For each color c in the local partition, 
%%   sum the degree of the nodes of that color 
%%   (where the degree of a node is equal to the number of edges it belongs to).
%% - Return the node count and degree of each color in the local partition.

start() ->
    Partitions = parse_input("../input.txt"),

    

start(_Input_file_path, _Part_a_output_file_path, _Part_b_output_file_path) -> 
    ok.

%%%%%%%%%%%%%%%%
%% gen_server


init(Args) ->
    {ok, Nodes}.

handle_call(Message, From, Nodes)


%%%%%%%%%%%%%%%%
%% Input parsing

parse_input(Path) ->
    {ok, File} = file:open(Path, read),
    Partitions = make_partitions(File, maps:new(), nil, 1),
    file:close(File),
    Partitions.

make_partitions(File, Partitions, PartitionName, Step) -> 
    case file:read_line(File) of
	{ok, Line} -> make_partitions(File, Line, Partitions, PartitionName, Step);
	eof -> Partitions
    end.

%% STEP 1:
%% Create a partition in the dictionary
make_partitions(File, Line, Partitions, _, 1) ->
    "partition " ++ PartitionName = string:trim(Line),
    make_partitions(File, Partitions, PartitionName, 2);

%% STEP 2:
%% Create Nodes in the partition
make_partitions(File, Line, Partitions, PartitionName, 2) ->    
    NodeNames = string:split(string:trim(Line), ",", all),
    Nodes = lists:map(fun(N) -> #node{id = N} end, NodeNames),
    UpdatedPartitions = maps:put(PartitionName, Nodes, Partitions),
    make_partitions(File, UpdatedPartitions, PartitionName, 3);

%% STEP 3:
%% Update colors in the partition nodes
make_partitions(File, Line, Partitions, PartitionName, 3) -> 
    Colors = string:split(string:trim(Line), ",", all),
    Nodes = maps:get(PartitionName, Partitions),
    UpdatedNodes = lists:map(fun({Color, Node}) -> Node#node{color = Color} end,
		     lists:zip(Colors, Nodes)),
    UpdatedPartitions = maps:update(PartitionName, UpdatedNodes, Partitions),
    make_partitions(File, UpdatedPartitions, PartitionName, 4);

%% STEP 4: 
%% Update edges in the partition nodes
make_partitions(File, Line, Partitions, PartitionName, 4) -> 
    Pairs = string:split(string:trim(Line), " ", all),
    Edges = lists:foldl(fun(Pair, Acc) -> 
				[NodeId, Edge] = string:split(Pair, ","),
				dict:append(NodeId, Edge, Acc) end, 
			dict:new(), 
			Pairs),
    Nodes = maps:get(PartitionName, Partitions),
    UpdatedNodes = lists:map(fun(#node{id = Id} = Node) ->
				     case dict:is_key(Id, Edges) of
					 true -> 
					     NodeEdges = dict:fetch(Id, Edges),
					     Node#node{edges = NodeEdges};
					 _ ->
					     Node
				     end
			     end, Nodes),
    UpdatedPartitions = maps:update(PartitionName, UpdatedNodes, Partitions),  
    make_partitions(File, UpdatedPartitions, nil, 1). 


    
			  

    
    
    
