%% @author Safwan Kamarrudin <shaihulud@alumni.cmu.edu>
%% @copyright 2012 Safwan Kamarrudin.
%% @doc General-purpose helper functions.

-module(filmo_utils).
-export([round_rating/1, filter_list/2, average/1, movie_to_proplist/1]).

-include("movie.hrl").

round_rating(Rating) ->
	case Rating of
		0 -> 0;
		_ -> 
			[RoundedRating] = io_lib:format("~.2f", [Rating]),
			RoundedRating
	end.

filter_list(Pattern, List) ->
	Eval = fun(S) -> 
		   	   {ok, T, _} = erl_scan:string(S), 
		   	   {ok,[A]} = erl_parse:parse_exprs(T), 
		   	   {value, V, _} = erl_eval:expr(A,[]), V 
		   end,
	FilterGen = fun(X) -> 
					Eval(lists:flatten(["fun(",X,")->true;(_)->false end."])) 
				end,
	lists:filter(FilterGen(Pattern),List).

average([]) -> 0;
average(Numbers) ->
	average(Numbers, 0, 0).

average([H|T], Length, Sum) ->
    average(T, Length + 1, Sum + H);
average([], Length, Sum) ->
    Sum / Length.

movie_to_proplist(#movie{} = Movie) ->
    [{K, element(I, Movie)} || {K, I} <- lists:zip(record_info(fields, movie), lists:seq(2, record_info(size, movie)))].


