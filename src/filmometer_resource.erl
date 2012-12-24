%% @author Safwan Kamarrudin <shaihulud@alumni.cmu.edu>
%% @copyright 2012 Safwan Kamarrudin.
%% @doc Look up movie ratings and average them out.

-module(filmometer_resource).
-export([init/1, content_types_provided/2, to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

-record(movie, {title,year,actors,poster,rating}).

init([]) -> 
	{ok, undefined}.

content_types_provided(ReqData, Context) ->
	{[{"application/json", to_json}], ReqData, Context}.

to_json(ReqData, Context) ->
    Title = wrq:get_qs_value("title", ReqData),
    Results = get_results_for(Title),
    CombinedResult = combine_results(Results),
    {CombinedResult, ReqData, Context}.

get_results_for(Title) ->
	%% TODO: Figure out how to retrieve results from a dynamic list of sources
    OMDBResult = get_omdb_result(Title),
    FlixsterResult = get_flixster_result(Title),
    TMDBResult = get_tmdb_result(Title),
    OMDBResult++FlixsterResult++TMDBResult.

get_omdb_result(SearchTitle) ->
	EncodedTitle = mochiweb_util:urlencode([{"t", SearchTitle}]),
    RequestUri = string:concat("http://www.omdbapi.com/?", EncodedTitle),
    {ok, RequestId} = httpc:request(get, {RequestUri, []}, [], [{sync, false}]),
    Result = wait_for_response(RequestId),

    {{_Version, 200, _ReasonPhrase}, _Headers, Body} = Result,
    ParsedJsonResult = mochijson:decode(Body),
    
    {_,[{_,Title},{_,Year},_Rated,_Released,_Runtime,_Genre,_Director,_Writer,{_,Actors},_Plot,{_,Poster},{_,Rating},_Votes,_ID,_Response]} = ParsedJsonResult,
    {ConvertedRating,_} = string:to_float(Rating),
    [#movie{title=Title,year=Year,actors=Actors,poster=Poster,rating=ConvertedRating}].

get_flixster_result(SearchTitle) ->
	APIKey = "b2x78beenefg6tq3ynr56r4a",
	PageLimit = 1,
	EncodedTitle = mochiweb_util:urlencode([{"q", SearchTitle}]),
	RequestUri = lists:flatten(
				 	io_lib:format("http://api.rottentomatoes.com/api/public/v1.0/movies.json?apikey=~s&page_limit=~p&~s", 
						          [APIKey, PageLimit, EncodedTitle])),
	{ok, RequestId} = httpc:request(get, {RequestUri, []}, [], [{sync, false}]),
	Result = wait_for_response(RequestId),

	{{_Version, 200, _ReasonPhrase}, _Headers, Body} = Result,
	ParsedJsonResult = mochijson:decode(Body),
	{struct,[_Total,{_Movies,{array,[{struct,Movie}]}},_Links,_LinkTemplate]} = ParsedJsonResult,
	build_flixster_result_from(Movie).

build_flixster_result_from(Movie) ->
	Title = proplists:get_value("title", Movie),
	Year = proplists:get_value("year", Movie),

	{array, AbridgedCast} = proplists:get_value("abridged_cast", Movie),
	Actors = [Actor || {struct,[{_,Actor}|_]} <- AbridgedCast],
	CombinedActors = string:join(Actors, ", "),

	{struct, Posters} = proplists:get_value("posters", Movie),
	OriginalPoster = proplists:get_value("original", Posters),

	{struct, Ratings} = proplists:get_value("ratings", Movie),
	CriticsScore = proplists:get_value("critics_score", Ratings),
	AudienceScore = proplists:get_value("audience_score", Ratings),
	AverageRating = (CriticsScore + AudienceScore) / 2 / 10,

	[#movie{title=Title,year=Year,actors=CombinedActors,poster=OriginalPoster,rating=AverageRating}].

get_tmdb_result(SearchTitle) ->
	APIKey = "8abd8211399f1196bdefef458fc4c5ed",
	EncodedTitle = mochiweb_util:urlencode([{"query", SearchTitle}]),
	RequestUri = lists:flatten(
				 	io_lib:format("http://api.themoviedb.org/3/search/movie?api_key=~s&~s", 
						          [APIKey, EncodedTitle])),

	{ok, RequestId} = httpc:request(get, {RequestUri, [{"Accept", "application/json"}]}, [], [{sync, false}]),
	Result = wait_for_response(RequestId),

	{{_Version, 200, _ReasonPhrase}, _Headers, Body} = Result,
	ParsedJsonResult = mochijson:decode(Body),

	{struct,[_Page,{_Results,{array,[FirstResult|_]}},_TotalPages,_TotalResults]} = ParsedJsonResult,
	{struct,[_Adult,_Backdrop,_Id,_OriginalTitle,{_,ReleaseDate},{_,Poster},_Popularity,{_,Title},{_,Rating},_Votes]} = FirstResult,
	[Year,_Month,_Day] = string:tokens(ReleaseDate,"-"),
	[#movie{title=Title,year=Year,actors="",poster=Poster,rating=Rating}].

combine_results(Results) ->
	Ratings = [Movie#movie.rating || Movie <- Results],
    AverageRating = round_rating(average(Ratings)),
    
    ConvertedResults = [{struct, movie_to_proplist(Movie)} || Movie <- Results],
    CombinedResult = {array,[{struct,[{"AverageRating",AverageRating}]} | ConvertedResults]},
    mochijson:encode(CombinedResult).

 wait_for_response(RequestId) ->
 	receive 
 		{http, {RequestId, Result}} -> Result 
 	after 
 		5000 -> timeout 
 	end.

%% Utility functions (consider moving them elsewhere)

round_rating(Rating) ->
	[RoundedRating] = io_lib:format("~.2f", [Rating]),
	RoundedRating.

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

average(Numbers) ->
	average(Numbers, 0, 0).

average([H|T], Length, Sum) ->
    average(T, Length + 1, Sum + H);
average([], Length, Sum) ->
    Sum / Length.

movie_to_proplist(#movie{} = Movie) ->
    [{K, element(I, Movie)} || {K, I} <- lists:zip(record_info(fields, movie), lists:seq(2, record_info(size, movie)))].
