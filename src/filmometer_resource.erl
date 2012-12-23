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
    erlang:display(OMDBResult),
    TomatoesResult = get_tomatoes_result(Title),
    erlang:display(TomatoesResult),
    {TomatoesResult, ReqData, Context}.

get_omdb_result(SearchTitle) ->
	EncodedTitle = mochiweb_util:urlencode([{"t", SearchTitle}]),
    RequestUri = string:concat("http://www.omdbapi.com/?", EncodedTitle),
    {ok, RequestId} = httpc:request(get, {RequestUri, []}, [], [{sync, false}]),
    Result = wait_for_response(RequestId),
    {{_Version, 200, _ReasonPhrase}, _Headers, Body} = Result,
    ParsedJsonResult = mochijson:decode(Body),
    {_,[Title,Year,_Rated,_Released,_Runtime,_Genre,_Director,_Writer,Actors,_Plot,Poster,{_,Rating},_Votes,_ID,_Response]} = ParsedJsonResult,
    mochijson:encode({struct,[Title,Year,Actors,Poster,{"Rating",Rating}]}).

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
	[_Id,Title,Year,_Rating,_Runtime,_Consensus,_ReleaseDates,{_Ratings,{struct,[_,{_,CriticsScore},_,{_,AudienceScore}]}},_Synopsis,Posters,AbridgedCast,_AlternateIDs,_Links2] = Movie,
	AverageRating = (CriticsScore + AudienceScore) / 2 / 10,
	mochijson:encode({struct,[Title,Year,AbridgedCast,Posters,{"Rating",AverageRating}]}).

 wait_for_response(RequestId) ->
 	receive 
 		{http, {RequestId, Result}} -> Result 
 	after 
 		5000 -> timeout 
 	end.