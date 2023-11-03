-module(graph_supervisor).
-behaviour(supervisor).
-export([start_link/2, add_partition/2]).
-export([init/1]).

start_link(ResultAPath, ResultBPath) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [ResultAPath, ResultBPath]).

add_partition(Id, Nodes) ->
    ChildSpec = #{id => Id,
	       start => {graph_partition, start_link, [Nodes]},
	       restart => permanent,
	       shutdown => 5000,
	       type => worker,
	       modules => [graph_partition]},
    supervisor:start_child(?MODULE, ChildSpec).


init([ResultAPath, ResultBPath]) ->
    SupFlags = #{strategy => one_for_one,
		 intensity => 1,
		 period => 5},
    ChildSpecs = [
                  #{id => "results_reporter",
                  start => {results_reporter, start_link, [ResultAPath, ResultBPath]},
                  restart => permanent,
                  shutdown => 5000,
                  type => worker,
                  modules => [graph_partition]}  
                 ],
    {ok, {SupFlags, ChildSpecs}}.


