-module(bs_protocol).

-export ([parse/1]).

parse(RawData) -> 
	Message = binary_to_list(RawData),
	{_Command,[_|_Nick]} = lists:splitwith(fun(T) -> [T] =/= ":" end ,Message).