%% @author Safwan Kamarrudin <shaihulud@alumni.cmu.edu>
%% @copyright 2012 Safwan Kamarrudin.
%% @doc OMDB source.

-module(omdb_source).
-export([get_result/2]).

-include("movie.hrl").

get_result(Criteria, Pid) ->
	SearchTitle = proplists:get_value("title", Criteria),
	EncodedTitle = mochiweb_util:urlencode([{"t", SearchTitle}]),
	SearchYear = proplists:get_value("year", Criteria),

	BaseUri = "http://www.omdbapi.com/?" ++ EncodedTitle,
    RequestUri = case SearchYear of 
    				undefined -> 
    					BaseUri;
    				_ -> 
    					BaseUri ++ "&y=" ++ SearchYear
    			 end,
    {ok, Result} = httpc:request(get, {RequestUri, []}, [], [{sync, true}]),

    {{_Version, 200, _ReasonPhrase}, _Headers, Body} = Result,
    ParsedJsonResult = serializer:deserialize(Body, json),

    case ParsedJsonResult of 
    	{struct, [{_, _Response}, {_, _Error}]} -> 
    		Pid ! [];
    	ParsedJsonResult ->
		    {_, [{_, Title}, {_, Year}, _Rated, _Released, _Runtime, {_, Genre},
			     _Director, _Writer, {_, Actors}, {_, Plot}, {_, Poster}, {_, Rating},
			     _Votes, _ID, _Type, _Response]} = ParsedJsonResult,

			{ConvertedYear, _} = string:to_integer(Year),
		    ConvertedRating = case string:to_float(Rating) of
		    				  	{error,_} -> 0;
		    				  	{R, _} -> R
		    				  end,
		    Pid ! [#movie{source="OMDB", title=Title, year=ConvertedYear, actors=Actors, 
		            poster=Poster, rating=ConvertedRating, genre=Genre, plot=Plot}]
	end.