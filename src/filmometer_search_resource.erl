%% @author Safwan Kamarrudin <shaihulud@alumni.cmu.edu>
%% @copyright 2012 Safwan Kamarrudin.
%% @doc Look up movie ratings and average them out.

-module(filmometer_search_resource).
-export([init/1, content_types_provided/2, to_json/2, combine_results/1, get_verdict_for/1]).

-include_lib("webmachine/include/webmachine.hrl").
-include("movie.hrl").

init([]) -> 
	{ok, undefined}.

content_types_provided(ReqData, Context) ->
	{[{"application/json", to_json}], ReqData, Context}.

to_json(ReqData, Context) ->
  Title = wrq:get_qs_value("title", ReqData),
  Year = wrq:get_qs_value("year", ReqData),
  Results = get_results_for([{"title", Title}, {"year", Year}]),
  CombinedResult = combine_results(Results),
  SerializedResult = serializer:serialize(CombinedResult, json),
  {SerializedResult, ReqData, Context}.

get_results_for(Criteria) ->
	%% TODO: Figure out how to retrieve results from a dynamic list of sources
	Pid = self(),
  spawn(omdb_source, get_result, [Criteria, Pid]),
  spawn(flixster_source, get_result, [Criteria, Pid]),
  spawn(tmdb_source, get_result, [Criteria, Pid]),
  Results = wait_for_responses(3, []),
  Results.

wait_for_responses(0, Results) -> Results;
wait_for_responses(Count, Results) ->
	receive
		Result -> wait_for_responses(Count-1, Results++Result)
	after 
 		30000 -> timeout %% TODO: store these in config/env variable
 	end.

combine_results(Results) ->
	case [OMDBSource || OMDBSource = #movie{source="OMDB"} <- Results] of
		[] -> [];
		AuthoritativeResults ->
			CombinedResults = combine_results(AuthoritativeResults, Results, []),
			{struct, [{"movies", {array, CombinedResults}}]}
	end.

combine_results([], _, CombinedResults) -> CombinedResults;
combine_results([H|T], AllResults, CombinedResults) ->
	AuthoritativeSource = H,
	ReferenceTitle = AuthoritativeSource#movie.title,
	ReferenceYear = AuthoritativeSource#movie.year,
	FilterFun = fun(M, Title, Year) ->
								(M#movie.title =:= Title) andalso (M#movie.year =:= Year)
							end,
	FilteredResults = [M || M <- AllResults, FilterFun(M, ReferenceTitle, ReferenceYear)],

	Ratings = [M#movie.rating || M <- FilteredResults, M#movie.rating > 0],
  AverageRating = filmo_utils:round_rating(filmo_utils:average(Ratings)),
  Verdict = get_verdict_for(AverageRating),

  NonEmptyActorsFun = fun(M) -> M#movie.actors =/= [] end,
  Actors = lists:nth(1, [M#movie.actors || M <- FilteredResults, NonEmptyActorsFun(M)]),

  %% Heroku doesn't like images from OMDB for some reason, so default to the first non-empty poster
  PosterFun = fun(M) -> 
  							M#movie.source =/= "OMDB" andalso M#movie.source =/= [] 
  						end,
	NonOMDBPosters = [M#movie.poster || M <- FilteredResults, PosterFun(M)],
  Poster = if
					   length(NonOMDBPosters) =:= 0 -> "/img/no_result.jpg";
			    	 true -> lists:nth(1, NonOMDBPosters)
			     end,

	Genre = AuthoritativeSource#movie.genre,
	Plot = AuthoritativeSource#movie.plot,
    
  ConvertedResults = [{struct, filmo_utils:movie_to_proplist(Movie)} || Movie <- FilteredResults],
  Result = {struct, [{"averageRating", AverageRating}, 
					    		   {"actors", Actors},
							       {"verdict", Verdict},
							       {"poster", Poster},
							       {"genre", Genre},
							       {"plot", Plot},
								   	 {"ratings", {array, ConvertedResults}}]},
	combine_results(T, AllResults, CombinedResults++[Result]).

get_verdict_for(Score) ->
	if
		Score >= 8 ->
			"Hellz yeah!";
		Score >= 7 ->
			"Uhmm, yeah";
		Score >= 6 ->
			"Meh";
		Score >= 4 ->
			"I pity the fools who watched this";
		Score >= 2 ->
			"Avoid like the plague";
		true ->
			"There should be a crime against watching this"
	end.
