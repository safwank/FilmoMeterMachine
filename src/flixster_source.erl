%% @author Safwan Kamarrudin <shaihulud@alumni.cmu.edu>
%% @copyright 2012 Safwan Kamarrudin.
%% @doc Flixster source.

-module(flixster_source).
-export([get_result/1]).

-include("movie.hrl").

get_result(Criteria) ->
	APIKey = "b2x78beenefg6tq3ynr56r4a",
	PageLimit = 5,

	SearchTitle = proplists:get_value("title", Criteria),
	EncodedTitle = mochiweb_util:urlencode([{"q", SearchTitle}]),
	
	RequestUri = lists:flatten(
				 	io_lib:format("http://api.rottentomatoes.com/api/public/v1.0/movies.json?apikey=~s&page_limit=~p&~s", 
						          [APIKey, PageLimit, EncodedTitle])),
	{ok, RequestId} = httpc:request(get, {RequestUri, []}, [], [{sync, false}]),
	Result = http_utils:wait_for_response(RequestId),

	{{_Version, 200, _ReasonPhrase}, _Headers, Body} = Result,
	ParsedJsonResult = serializer:deserialize(Body, json),
	{struct, [_Total, {_Movies, {array, Movies}}, _Links, _LinkTemplate]} = ParsedJsonResult,
	build_result_from(Movies).

build_result_from(Movies) ->
	CreateMovieFun = 
		fun(MovieStruct, MovieAcc) ->
			{struct, Movie} = MovieStruct,
			Title = proplists:get_value("title", Movie),
			Year = proplists:get_value("year", Movie),

			{array, AbridgedCast} = proplists:get_value("abridged_cast", Movie),
			Actors = [Actor || {struct,[{_,Actor}|_]} <- AbridgedCast],
			CombinedActors = string:join(Actors, ", "),

			{struct, Posters} = proplists:get_value("posters", Movie),
			DetailedPoster = proplists:get_value("detailed", Posters),

			{struct, Ratings} = proplists:get_value("ratings", Movie),
			CriticsScore = proplists:get_value("critics_score", Ratings),
			AudienceScore = proplists:get_value("audience_score", Ratings),
			AverageRating = (CriticsScore + AudienceScore) / 2 / 10,

			[#movie{source="Flixster", title=Title, year=Year, actors=CombinedActors,
					poster=DetailedPoster, rating=AverageRating}] ++ MovieAcc
		end,
	
	MovieList = lists:foldl(CreateMovieFun, [], Movies),
	MovieList.

