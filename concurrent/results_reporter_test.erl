-module(results_reporter_test).
-export([test/0]).

test() ->
    {ok, _Pid} = results_reporter:start_link("../results_a_test.txt", "../results_b_test.txt"),
    
    TestData = [{"green", 4, 4},
                 {"orange", 4, 4},
                 {"purple", 4, 4},
                 {"green", 4, 4},
                 {"orange", 4, 4},
                 {"purple", 4, 4},                
                 {"green", 4, 4},
                 {"orange", 4, 4},
                 {"purple", 4, 4},                
                 {"green", 4, 4},
                 {"orange", 4, 4},
                 {"purple", 4, 4},            
                 {"green", 4, 4},
                 {"orange", 4, 4},
                 {"purple", 4, 4}
                ],

    lists:foreach(fun({Color, Count, Degree}) -> 
                             results_reporter:add_color_count_and_degree(Color, Count, Degree)
                  end, TestData),
    
    results_reporter:write_color_count_and_degrees().
    
    
    
