-module(graph_partition_test).   
-export([get_color_count_and_degree_test/0]).


get_color_count_and_degree_test() -> 
    results_reporter:start_link("../testA", "../testB"),
    StartPartition = fun(Nodes) ->
        {ok, Pid} = graph_partition:start_link(Nodes), 
        Pid 
    end,
    [Pid | _] = lists:map(StartPartition, test_data:get()),
    erlang:display(sys:get_state(Pid)).
    
    

