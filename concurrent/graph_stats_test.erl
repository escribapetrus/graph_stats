-module(graph_stats_test).
-export([test_data/0]).


test_data() ->
%%    graph_supervisor:start_link("../testfile_a.txt", "../testfile_b.txt"),
    graph_stats:start().
