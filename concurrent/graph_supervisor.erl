-module(graph_supervisor).
-behaviour(supervisor).
-export([start_link/2, add_partition/2, calculate_most_influential_nodes/0,
        fetch_degree/1]).
-export([init/1]).
-export([test_calculate_most_influential_nodes/0,
         test_fetch_degree/0]).

start_link(ResultAPath, ResultBPath) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [ResultAPath, ResultBPath]).

add_partition(Id, Nodes) ->
    ChildSpec = #{id => Id,
	       start => {graph_partition, start_link, [Nodes]},
	       restart => permanent,
	       shutdown => 5000,
	       type => worker},
    supervisor:start_child(?MODULE, ChildSpec).

calculate_most_influential_nodes() ->
    Children = supervisor:which_children(?MODULE),
    SendMessage = fun({_Id, Pid, _, _}) -> 
                          graph_partition:most_influential_nodes(Pid) end,
    lists:foreach(SendMessage, Children).

fetch_degree(NodeId) ->
    Children = supervisor:which_children(?MODULE),
    fetch_degree(NodeId, Children).

fetch_degree(NodeId, []) -> {NodeId, 0};
fetch_degree(NodeId, [{"partition" ++ _, Pid, _, _}|Rest]) ->
    case graph_partition:degree(Pid, NodeId) of
        0 -> fetch_degree(NodeId, Rest);
        Value -> {NodeId, Value}
    end;
fetch_degree(NodeId, [_|Rest]) -> fetch_degree(NodeId, Rest).
    

init([ResultAPath, ResultBPath]) ->
    SupFlags = #{strategy => one_for_one,
		 intensity => 1,
		 period => 5},
    ChildSpecs = [
                  #{id => "results_reporter",
                  start => {results_reporter, start_link, [ResultAPath, ResultBPath]},
                  restart => permanent,
                  shutdown => 5000,
                  type => worker}  
                 ],
    {ok, {SupFlags, ChildSpecs}}.


%%%%%%%%%%%% TESTS

test_calculate_most_influential_nodes() ->
    graph_supervisor:start_link("../test_graph_supervisor_a.txt", "../test_graph_supervisor_b"),
    lists:foreach(fun({Id, Nodes}) -> add_partition(Id, Nodes) end, test_data_nodes()),
    calculate_most_influential_nodes().

test_fetch_degree() -> 
    graph_supervisor:start_link("../test_graph_supervisor_a.txt", "../test_graph_supervisor_b"),
    lists:foreach(fun({Id, Nodes}) -> add_partition(Id, Nodes) end, test_data_nodes()),
    true = {9, 4} == fetch_degree(9),
    true = {1, 4} == fetch_degree(1),
    true = {12, 0} == fetch_degree(12).


test_data_nodes() -> 
    [{"partition 0", [
      {node,0,"blue",[1,2,3,9]},
      {node,1,"green",[2,3,4,0]},
      {node,2,"blue",[3,0,1]},
      {node,3,"red",[0,1,2]}]},
     {"partition 1", [
      {node,4,"green",[5,6,7,1]},
      {node,5,"red",[6,7,8,4]},
      {node,6,"green",[7,4,5]},
      {node,7,"green",[4,5,6]}]},
     {"partition 2", [
      {node,8,"red",[9,10,11,5]},
      {node,9,"blue",[10,11,0,8]},
      {node,10,"blue",[11,8,9]},
      {node,11,"red",[8,9,10]}]}].
