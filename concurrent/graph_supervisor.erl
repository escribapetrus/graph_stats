-module(graph_supervisor).
-behaviour(supervisor).
-export([start_link/2, add_partition/2, 
         calculate_most_influential_nodes/0,
         neighbors/1]).
-export([init/1]).
-export([test_calculate_most_influential_nodes/0,

        test_data_nodes/0]).

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
    lists:map(fun({"partition" ++ _, Pid, _, _}) -> 
                          graph_partition:most_influential_nodes(Pid); 
                     (_) -> 
                          do_nothing
                  end, Children).

neighbors(PartitionPid) ->
    neighbors(PartitionPid, supervisor:which_children(?MODULE), []).

neighbors(_, [], Acc) -> Acc;
neighbors(PartitionPid, [{"partition" ++ _, Pid, _,_}|Rest], Acc) when PartitionPid == Pid ->
    neighbors(PartitionPid, Rest, Acc);
neighbors(PartitionPid, [{"partition" ++ _, Pid, _,_}|Rest], Acc) ->
    neighbors(PartitionPid, Rest, [Pid|Acc]);
neighbors(PartitionPid, [_| Rest], Acc) -> 
    neighbors(PartitionPid, Rest, Acc).


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
    {ok, _} = start_link("../test_graph_supervisor_a.txt", "../test_graph_supervisor_b"),
    lists:foreach(fun({Id, Nodes}) -> add_partition(Id, Nodes) end, test_data_nodes()),
    calculate_most_influential_nodes().

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
