-module(graph_supervisor).
-behaviour(supervisor).
-export([start_link/0, add_partition/2]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

add_partition(Id, Nodes) ->
    ChildSpec = #{id => Id,
	       start => {graph_partition, start_link, [Nodes]},
	       restart => permanent,
	       shutdown => 5000,
	       type => worker,
	       modules => [graph_partition]},
    supervisor:start_child(?MODULE, ChildSpec).


init([]) ->
    SupFlags = #{strategy => one_for_one,
		 intensity => 1,
		 period => 5},
    {ok, {SupFlags, []}}.


