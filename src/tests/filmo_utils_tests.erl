-module(filmo_utils_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

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

average_should_return_zero_if_the_list_is_empty_test() ->
	Average = filmo_utils:average([]),
	?assert(Average =:= 0).

average_should_just_return_a_number_if_it_is_the_only_item_in_the_list_test() ->
	Average = filmo_utils:average([6.6]),
	?assert(Average =:= 6.6).

average_should_return_the_average_of_a_list_of_numbers_test() ->
	Average = filmo_utils:average([6, 10, 20, 7]),
	?assert(Average =:= 10.75).

-endif.