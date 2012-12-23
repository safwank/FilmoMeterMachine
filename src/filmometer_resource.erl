%% @author Safwan Kamarrudin <shaihulud@alumni.cmu.edu>
%% @copyright 2012 Safwan Kamarrudin.
%% @doc Example webmachine_resource.

-module(filmometer_resource).
-export([init/1, content_types_provided/2, to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> 
	{ok, undefined}.

content_types_provided(ReqData, Context) ->
	{[{"application/json", to_json}], ReqData, Context}.

to_json(ReqData, Context) ->
    Title = wrq:get_qs_value("title", ReqData),
    OMDBResult = get_omdb_result(Title),
    TomatoesResult = get_tomatoes_result(Title),
    CombinedResult = combine_results([OMDBResult,TomatoesResult]),
    EncodedCombinedResult = mochijson:encode(CombinedResult),
    {EncodedCombinedResult, ReqData, Context}.

get_omdb_result(SearchTitle) ->
	EncodedTitle = mochiweb_util:urlencode([{"t", SearchTitle}]),
    RequestUri = string:concat("http://www.omdbapi.com/?", EncodedTitle),
    {ok, RequestId} = httpc:request(get, {RequestUri, []}, [], [{sync, false}]),
    Result = wait_for_response(RequestId),

    {{_Version, 200, _ReasonPhrase}, _Headers, Body} = Result,
    ParsedJsonResult = mochijson:decode(Body),
    
    {_,[Title,Year,_Rated,_Released,_Runtime,_Genre,_Director,_Writer,Actors,_Plot,Poster,{_,Rating},_Votes,_ID,_Response]} = ParsedJsonResult,
    {struct,[Title,Year,Actors,Poster,{"Rating",Rating}]}.

get_tomatoes_result(SearchTitle) ->
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
	build_tomatoes_result_from(Movie).

build_tomatoes_result_from(Movie) ->
	Title = proplists:get_value("title", Movie),
	Year = proplists:get_value("year", Movie),
	Actors = proplists:get_value("abridged_cast", Movie),
	Posters = proplists:get_value("posters", Movie),
	RatingsStruct = proplists:get_value("ratings", Movie),
	{struct,Ratings} = RatingsStruct,
	CriticsScore = proplists:get_value("critics_score", Ratings),
	AudienceScore = proplists:get_value("audience_score", Ratings),
	AverageRating = (CriticsScore + AudienceScore) / 2 / 10,
	{struct,[{"Title",Title},{"Year",Year},{"Actors",Actors},{"Posters",Posters},{"Rating",AverageRating}]}.

combine_results([OMDBResult, TomatoesResult]) ->
	{_,[_,_,_,_,{_,OMDBRating}]} = OMDBResult,
    {ConvertedOMDBRating,_} = string:to_float(OMDBRating),
    {_,[_,_,_,_,{_,TomatoesRating}]} = TomatoesResult,
    AverageRating = (ConvertedOMDBRating + TomatoesRating) / 2,
    {array,[{struct,[{"AverageRating", round_rating(AverageRating)}]},OMDBResult,TomatoesResult]}.

 wait_for_response(RequestId) ->
 	receive 
 		{http, {RequestId, Result}} -> Result 
 	after 
 		5000 -> timeout 
 	end.

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