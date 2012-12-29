-module(filmometer_search_resource_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-include("../movie.hrl").

combine_results_should_return_an_empty_list_if_there_is_no_result_test() ->
	Expected = [],
	Actual = filmometer_search_resource:combine_results([]),
	?assert(Actual =:= Expected).

combine_results_should_return_an_empty_list_if_there_is_no_result_from_omdb_test() ->
	Expected = [],
	Actual = filmometer_search_resource:combine_results([#movie{source="Flixster"}]),
	?assert(Actual =:= Expected).

combine_results_should_return_a_list_of_aggregated_results_if_there_is_a_result_from_omdb_test() ->
	PropLists = [{struct, [{source,"OMDB"}, {title,"Sicko"}, {year,2007}, 
						   {actors,"John Doe, Jane Doe"},
						   {poster,"foo.bar/omdb.jpg"}, {rating,8.8}]}, 
				 {struct, [{source,"TMDB"}, {title,"Sicko"}, {year,2007}, 
				 		   {actors,"John Doe, Jane Doe"},
				 		   {poster,"foo.bar/tmdb.jpg"}, {rating,9.0}]}],
	Expected = {struct, [{"averageRating", 8.9}, 
				         {"verdict", "Hellz yeah!"},
				         {"poster", "foo.bar/tmdb.jpg"},
				         {"ratings", {array, PropLists}}]},
	Actual = filmometer_search_resource:combine_results(
		[#movie{source="OMDB", title="Sicko", actors="John Doe, Jane Doe", 
				year=2007, rating=8.8, poster="foo.bar/omdb.jpg"},
		 #movie{source="TMDB", title="Sicko",  actors="John Doe, Jane Doe",
		 		year=2007, rating=9.0, poster="foo.bar/tmdb.jpg"}]),
	?assert(Actual =:= Expected).

get_verdict_for_should_return_the_correct_verdict_for_any_number_between_0_and_2_test() ->
	Expected = "There should be a crime against watching this",
	Actual = filmometer_search_resource:get_verdict_for(1.99),
	?assert(Actual =:= Expected).

get_verdict_for_should_return_the_correct_verdict_for_any_number_between_2_and_4_test() ->
	Expected = "Avoid like the plague",
	Actual = filmometer_search_resource:get_verdict_for(3.99),
	?assert(Actual =:= Expected).

get_verdict_for_should_return_the_correct_verdict_for_any_number_between_4_and_6_test() ->
	Expected = "I pity the fools who watched this",
	Actual = filmometer_search_resource:get_verdict_for(5.99),
	?assert(Actual =:= Expected).

get_verdict_for_should_return_the_correct_verdict_for_any_number_between_6_and_7_test() ->
	Expected = "Meh",
	Actual = filmometer_search_resource:get_verdict_for(6.99),
	?assert(Actual =:= Expected).

get_verdict_for_should_return_the_correct_verdict_for_any_number_between_7_and_8_test() ->
	Expected = "Uhmm, yeah",
	Actual = filmometer_search_resource:get_verdict_for(7.99),
	?assert(Actual =:= Expected).

get_verdict_for_should_return_the_correct_verdict_for_any_number_between_8_and_10_test() ->
	Expected = "Hellz yeah!",
	Actual = filmometer_search_resource:get_verdict_for(9.99),
	?assert(Actual =:= Expected).

-endif.