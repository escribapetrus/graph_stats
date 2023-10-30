-module(graph_partition_test).   
-export([get_color_count_and_degree_test/0]).

test_data() -> 
    [
     [{node,"0","blue",["1","2","3","9"]},{node,"1","green",["2","3","4","0"]},{node,"2","blue",["3","0","1"]},{node,"3","red",["0","1","2"]}],
     [{node,"4","green",["5","6","7","1"]},{node,"5","red",["6","7","8","4"]},{node,"6","green",["7","4","5"]},{node,"7","green",["4","5","6"]}],
     [{node,"8","red",["9","10","11","5"]},{node,"9","blue",["10","11","0","8"]},{node,"10","blue",["11","8","9"]},{node,"11","red",["8","9","10"]}]
    ].

get_color_count_and_degree_test() -> 
    Pids = lists:map(fun(Nodes) ->
        {ok, Pid} = graph_partition:start_link(Nodes), 
        Pid 
    end,
    test_data()),

    lists:foreach(fun(Pid) ->
			     erlang:display(
 graph_partition:color_count_and_degree(Pid)
)
		  end,
		  Pids).
