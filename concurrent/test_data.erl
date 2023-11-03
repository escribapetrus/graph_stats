-module(test_data).
-export([nodes/0, color_counts_and_degrees/0]).

nodes() -> 
    [
     [
      {node,"0","blue",["1","2","3","9"]},
      {node,"1","green",["2","3","4","0"]},
      {node,"2","blue",["3","0","1"]},
      {node,"3","red",["0","1","2"]}
     ],
     [
      {node,"4","green",["5","6","7","1"]},
      {node,"5","red",["6","7","8","4"]},
      {node,"6","green",["7","4","5"]},
      {node,"7","green",["4","5","6"]}],
     [
      {node,"8","red",["9","10","11","5"]},
      {node,"9","blue",["10","11","0","8"]},
      {node,"10","blue",["11","8","9"]},
      {node,"11","red",["8","9","10"]}
     ]
    ].

color_counts_and_degrees() ->
    [
     [{"red",1,3},{"green",1,4},{"blue",2,7}],
     [{"red",1,4},{"green",3,10}],
     [{"red",2,7},{"blue",2,7}]
    ].
