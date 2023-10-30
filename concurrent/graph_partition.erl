-module(graph_partition).
-behaviour(gen_server).
-export([start_link/1, color_count_and_degree/1]).
-export([init/1, handle_call/3, handle_cast/2]).

-record(node, {id, color, edges = []}).
-record(color_count_and_degree, {color = nil, count = 0, degree = 0}).
-record(most_influential_node, {nodes = []}).

start_link(Nodes) ->
    gen_server:start_link(?MODULE, Nodes, []).

color_count_and_degree(Nodes) when is_list(Nodes) ->
    GroupByColor = fun(#node{color = Color}) -> Color end,
    CountEdges = fun(#node{edges = Edges}, Acc) -> length(Edges) + Acc end,
    ColorCountAndDegrees = fun(Color, ColorNodes, Acc) -> 
        ColorCountAndDegree = {
        Color,
        length(Nodes),
        lists:foldl(CountEdges, 0, ColorNodes)
    }, [ColorCountAndDegree | Acc] end,
    ColorGroups = maps:groups_from_list(GroupByColor, Nodes),
    maps:fold(ColorCountAndDegrees,[], ColorGroups);


color_count_and_degree(Pid) ->
    gen_server:call(Pid, color_count_and_degree).

init(Nodes) ->
    {ok, {Nodes, #color_count_and_degree{}, #most_influential_node{}}}.


handle_call(color_count_and_degree, _From, {Nodes, _, _}) ->
    Reply = color_count_and_degree(Nodes), 

    {reply, Reply, {Nodes}}.



handle_cast(_Request, State) ->
    {noreply, State}.

