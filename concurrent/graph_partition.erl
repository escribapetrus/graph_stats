-module(graph_partition).
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2]).
-export([
         color_count_and_degrees/1, 
         most_influential_nodes/1,
         degree/2]).
-export([
         test_color_count_and_degrees/0,
         test_most_influential_nodes/0,
         test_degree/0]).

-record(node, {id, color, edges = []}).

start_link(Nodes) ->
    gen_server:start_link(?MODULE, Nodes, []).

color_count_and_degrees(Pid) ->
    gen_server:cast(Pid, color_count_and_degrees).

most_influential_nodes(Pid) ->
    gen_server:cast(Pid, most_influential_nodes).

degree(Pid, NodeId) ->
    gen_server:call(Pid, {degree, NodeId}).

init(Nodes) ->
    {ok, Nodes}.

handle_call({degree, NodeId}, _From, Nodes) ->
    case lists:search(fun(N) -> N#node.id == NodeId end, Nodes) of
        false ->
            {reply, 0, Nodes};
        {value, Value} -> 
            {reply, length(Value#node.edges), Nodes}
    end;

handle_call(Message, _From, State) ->
    {reply, Message, State}.

handle_cast(most_influential_nodes, Nodes) ->
    %% - Get the degree of each internal node of Ap .
    %% - Find the list of external nodes connected to Ap . 
    %%   (Any node in the edge list that is not in the node list is an
    %%   external node of Ap )
    %% - For each external node n, message all other actors 
    %%   requesting the degree of node n. (An actor should return
    %%   0 if node n does not exist in their local partition).
    %% - Find the largest degree node(s) from among the 
    %%   internal and external nodes of actor Ap . If there are multiple
    %%   nodes of the same max degree, return all such nodes.
    NodeIds = [N#node.id || N <- Nodes],    
    Edges = lists:flatten([N#node.edges || N <- Nodes]),
    ExternalNodes = lists:filter(fun(X) -> not lists:member(X, NodeIds) end, Edges),
    Res = lists:foldl(fun(Elem, [{_NodeId, Degree}|_] = Acc) -> 
                        case graph_supervisor:fetch_degree(Elem) of
                            {NodeId, Value} when Value > Degree ->
                                [{NodeId, Value}];
                            {NodeId, Value} when Value == Degree ->
                                [{NodeId, Value}| Acc];
                            _ -> Acc
                                     
                            end
                end,
                [{0, 0}],
                NodeIds ++ ExternalNodes),
    erlang:display(Res),
    {noreply, Nodes};

handle_cast(color_count_and_degrees, Nodes) ->
    GroupByColor = fun(#node{color = Color}) -> Color end,
    CountEdges = fun(#node{edges = Edges}, Acc) -> length(Edges) + Acc end,
    ColorCountAndDegrees = fun(Color, ColorNodes, Acc) -> 
        ColorCountAndDegree = {
        Color,
        length(ColorNodes),
        lists:foldl(CountEdges, 0, ColorNodes)
    }, [ColorCountAndDegree | Acc] end,
    ColorGroups = maps:groups_from_list(GroupByColor, Nodes),
    maps:fold(ColorCountAndDegrees, [], ColorGroups),
    
    lists:foreach(fun({Color, Count, Degree}) -> 
                          results_reporter:add_color_count_and_degree(Color, Count, Degree)
                  end, ColorCountAndDegrees),
    
    {noreply, Nodes}.    


%%%%%%%%%%%%%%% TESTS

test_color_count_and_degrees() -> 
    results_reporter:start_link("../testA", "../testB"),
    StartPartition = fun(Nodes) ->
        {ok, Pid} = graph_partition:start_link(Nodes), 
        Pid 
    end,
    [Pid | _] = lists:map(StartPartition, test_data_nodes()),
    erlang:display(sys:get_state(Pid)).

test_most_influential_nodes() -> 
    results_reporter:start_link("../testA", "../testB"),
    StartPartition = fun(Nodes) ->
        {ok, Pid} = graph_partition:start_link(Nodes), 
        Pid 
    end,
    [Pid | _] = lists:map(StartPartition, test_data_nodes()),
    most_influential_nodes(Pid),
    ok.

test_degree() -> 
    [Pid | _] = lists:map(fun(Nodes) ->
        {ok, Pid} = graph_partition:start_link(Nodes), 
        Pid 
    end, test_data_nodes()),
    true = 4 == degree(Pid, 0),
    true = 3 == degree(Pid, 3),
    true = 0 == degree(Pid, 9),
    true = 0 == degree(Pid, 12).

test_data_nodes() ->
    [[{node,0,"blue",[1,2,3,9]},
      {node,1,"green",[2,3,4,0]},
      {node,2,"blue",[3,0,1]},
      {node,3,"red",[0,1,2]}],
     [{node,4,"green",[5,6,7,1]},
      {node,5,"red",[6,7,8,4]},
      {node,6,"green",[7,4,5]},
      {node,7,"green",[4,5,6]}],
     [{node,8,"red",[9,10,11,5]},
      {node,9,"blue",[10,11,0,8]},
      {node,10,"blue",[11,8,9]},
      {node,11,"red",[8,9,10]}]].


