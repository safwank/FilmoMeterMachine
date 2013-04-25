%% @author Safwan Kamarrudin <shaihulud@alumni.cmu.edu>
%% @copyright 2012 Safwan Kamarrudin.
%% @doc OMDB source.

-module(omdb_source).
-export([get_result/2, request_movie_details/2]).

-include("movie.hrl").

get_result(Criteria, Pid) ->
	SearchTitle = proplists:get_value("title", Criteria),
	EncodedTitle = mochiweb_util:urlencode([{"s", SearchTitle}]),
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
  ParsedSearchResults = serializer:deserialize(Body, json),

  case ParsedSearchResults of
  	{struct, [{_, _Response}, {_, _Error}]} -> 
  		Pid ! [];
  	ParsedSearchResults ->
  		{struct, [{_, {array, Results}}]} = ParsedSearchResults,
  		Movies = get_all_movie_details(Results),
  		Pid ! Movies
  end.

get_all_movie_details(Results) ->
	request_movie_details_async(Results),
	ResultCount = length(Results),
	Movies = wait_for_movie_details_responses(ResultCount, []),
	Movies.

request_movie_details_async([]) -> ok;
request_movie_details_async([H|T]) ->
	Pid = self(),
	{_, [{_, _Title}, {_, _Year}, {_, MovieID}, {_, _Type}]} = H,
	spawn(?MODULE, request_movie_details, [MovieID, Pid]),
	request_movie_details_async(T).

request_movie_details(MovieID, Pid) ->
	EncodedID = mochiweb_util:urlencode([{"i", MovieID}]),
	RequestUri = "http://www.omdbapi.com/?" ++ EncodedID,
	{ok, Result} = httpc:request(get, {RequestUri, []}, [], [{sync, true}]),

  {{_Version, 200, _ReasonPhrase}, _Headers, Body} = Result,
  ParsedResult = serializer:deserialize(Body, json),

  case ParsedResult of 
  	{struct, [{_, _Response}, {_, _Error}]} -> Pid ! [];
  	ParsedResult ->
	    {_, [{_, Title}, {_, Year}, _Rated, _Released, _Runtime, {_, Genre},
	     _Director, _Writer, {_, Actors}, {_, Plot}, {_, Poster}, {_, Rating},
	     _Votes, _ID, _Type, _Response]} = ParsedResult,

			{ConvertedYear, _} = string:to_integer(Year),
	    ConvertedRating = case string:to_float(Rating) of
					    				  	{error,_} -> 0;
					    				  	{R, _} -> R
					    				  end,
	    Pid ! [#movie{source="OMDB", title=Title, year=ConvertedYear, actors=Actors, 
	            poster=Poster, rating=ConvertedRating, genre=Genre, plot=Plot}]
	end.

wait_for_movie_details_responses(0, Results) -> Results;
wait_for_movie_details_responses(Count, Results) ->
	receive
		Result -> wait_for_movie_details_responses(Count-1, Results++Result)
	after 
 		25000 -> timeout 
 	end.