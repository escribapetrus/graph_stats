-module(graph_partition).
-behaviour(gen_statem).
-export([start_link/1]).
-export([init/1, callback_mode/0]).
-export([calculating/3, reporting/3, still/3]).

-record(node, {id, color, edges = []}).

start_link(Nodes) ->
    gen_statem:start_link(?MODULE, Nodes, []).

callback_mode() -> state_functions.

init(Nodes) ->
    io:format("init partition with nodes ~p~n", [Nodes]),
    {ok, calculating, {Nodes, [], []}}.

calculating(_EventType, _EventContent, {Nodes, _, MostInfluentialNodes}) ->
    io:format("~p: calculating color counts and degrees ~n", []),
    ColorCountsAndDegrees = color_count_and_degree(Nodes), 

    {next_state, reporting, {Nodes, ColorCountsAndDegrees, MostInfluentialNodes}}.

reporting(_EventType, _EventContent, {_, Result, _} = State) ->
    io:format("reporting color counts and degrees: ~p~n", [Result]),
    lists:foreach(fun({Color, Count, Degree}) -> 
                             results_reporter:add_color_count_and_degree(Color, Count, Degree)
                     end, Result), 
    {next_state, still, State}.

still(_EventType, _EventContent, _State) ->
    keep_state_and_data.

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


