%% @author Safwan Kamarrudin <shaihulud@alumni.cmu.edu>
%% @copyright 2012 Safwan Kamarrudin.
%% @doc OMDB source.

-module(omdb_source).
-export([get_result/1]).

-include("movie.hrl").

get_result(SearchTitle) ->
	EncodedTitle = mochiweb_util:urlencode([{"t", SearchTitle}]),
    RequestUri = string:concat("http://www.omdbapi.com/?", EncodedTitle),
    {ok, RequestId} = httpc:request(get, {RequestUri, []}, [], [{sync, false}]),
    Result = http_utils:wait_for_response(RequestId),

    {{_Version, 200, _ReasonPhrase}, _Headers, Body} = Result,
    ParsedJsonResult = mochijson:decode(Body),

    case ParsedJsonResult of 
    	{struct, [{_, _Response}, {_, _Error}]} -> [];
    	ParsedJsonResult ->
		    {_, [{_, Title}, {_,Year}, _Rated, _Released, _Runtime, _Genre,
			     _Director, _Writer, {_, Actors}, _Plot, {_, Poster}, {_, Rating},
			     _Votes, _ID, _Response]} = ParsedJsonResult,

			{ConvertedYear, _} = string:to_integer(Year),
		    ConvertedRating = case string:to_float(Rating) of
		    				  	{error,_} -> 0;
		    				  	{R, _} -> R
		    				  end,
		    [#movie{source="OMDB", title=Title, year=ConvertedYear, actors=Actors, poster=Poster, rating=ConvertedRating}]
	end.