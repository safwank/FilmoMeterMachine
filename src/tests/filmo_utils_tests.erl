-module(filmo_utils_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-include("../movie.hrl").

round_rating_should_return_zero_if_rating_is_zero_test() ->
	Rating = filmo_utils:round_rating(0),
	?assert(Rating =:= 0).

round_rating_should_round_a_number_with_one_decimal_place_test() ->
	Rating = filmo_utils:round_rating(2.1),
	?assert(Rating =:= 2.1).

round_rating_should_round_a_number_with_two_decimal_places_test() ->
	Rating = filmo_utils:round_rating(2.12),
	?assert(Rating =:= 2.12).

round_rating_should_round_a_number_with_more_than_two_decimal_places_test() ->
	RatingRoundedDown = filmo_utils:round_rating(2.124),
	?assert(RatingRoundedDown =:= 2.12),

	RatingRoundedUp = filmo_utils:round_rating(2.125),
	?assert(RatingRoundedUp =:= 2.13).

filter_list_should_return_an_empty_list_if_there_is_no_match_test() ->
	Expected = [],
	Actual = filmo_utils:filter_list("{foo, bar}", [{a, b}, {c, d}, {e, f}]),
	?assert(Actual =:= Expected).

filter_list_should_return_a_matching_list_if_there_is_a_match_test() ->
	Expected = [{foo, bar}, {foo, "baz"}],
	Actual = filmo_utils:filter_list("{foo, _}", [{foo, bar}, {foo, "baz"}, {e, f}]),
	?assert(Actual =:= Expected).

average_should_return_zero_if_the_list_is_empty_test() ->
	Average = filmo_utils:average([]),
	?assert(Average =:= 0).

average_should_just_return_a_number_if_it_is_the_only_item_in_the_list_test() ->
	Average = filmo_utils:average([6.6]),
	?assert(Average =:= 6.6).

average_should_return_the_average_of_a_list_of_numbers_test() ->
	Average = filmo_utils:average([6, 10, 20, 7]),
	?assert(Average =:= 10.75).

movie_to_proplist_should_return_an_empty_proplist_if_movie_is_empty_test() ->
	Expected = [{source,undefined},{title,undefined},{year,undefined},{actors,undefined},
				{poster,undefined},{rating,undefined},{genre,undefined},{plot,undefined}],
	Actual = filmo_utils:movie_to_proplist(#movie{}),
	?assert(Actual =:= Expected).

movie_to_proplist_should_return_a_valid_proplist_with_values_if_movie_is_not_empty_test() ->
	Expected = [{source, "OMDB"}, {title, "Fubar"}, {year, 2012}, 
				{actors, "John Doe, Jane Doe"}, {poster, "http://fu.bar/com.jpg"},
				{rating, 6.66}, {genre, "Comedy"}, {plot, "Some plot"}],
	Actual = filmo_utils:movie_to_proplist(
		#movie{source="OMDB", title="Fubar", year=2012, actors="John Doe, Jane Doe",
			   poster="http://fu.bar/com.jpg", rating=6.66, genre="Comedy", plot="Some plot"}),
	?assert(Actual =:= Expected).

-endif.