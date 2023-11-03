-module(graph_partition).
-behaviour(gen_server).
-export([start_link/1, color_count_and_degree/1]).
-export([init/1, handle_call/3, handle_cast/2]).

-record(node, {id, color, edges = []}).

start_link(Nodes) ->
    gen_server:start_link(?MODULE, Nodes, []).

init(Nodes) ->
    gen_server:cast(self(), get_color_count_and_degree),
    {ok, {Nodes, [], []}}.

handle_cast(get_color_count_and_degree, {Nodes, _, MostInfluentialNodes}) ->
    ColorCountsAndDegrees = color_count_and_degree(Nodes), 
    gen_server:cast(self(), push_color_count_and_degree),
    {noreply, {Nodes, ColorCountsAndDegrees, MostInfluentialNodes}};

handle_cast(push_color_count_and_degree, {_, Result, _} = State) ->
    lists:foreach(fun({Color, Count, Degree}) -> 
                             results_reporter:add_color_count_and_degree(Color, Count, Degree)
                     end, Result), 
    {noreply, State}.

handle_call(Message, _From, State) ->
    {reply, Message, State}.

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
    maps:fold(ColorCountAndDegrees, [], ColorGroups).