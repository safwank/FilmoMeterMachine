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
    %% TODO: Include year
    Results = get_results_for(Title),
    CombinedResult = combine_results(Results),
    {CombinedResult, ReqData, Context}.

get_results_for(Title) ->
	%% TODO: Figure out how to retrieve results from a dynamic list of sources
    OMDBResult = omdb_source:get_result(Title),
    FlixsterResult = flixster_source:get_result(Title),
    TMDBResult = tmdb_source:get_result(Title),
    OMDBResult++FlixsterResult++TMDBResult.

combine_results(Results) ->
	case [OMDBSource || OMDBSource = #movie{source="OMDB"} <- Results] of
		[] -> [];
		[AuthoritativeSource] ->
			ReferenceTitle = AuthoritativeSource#movie.title,
			ReferenceYear = AuthoritativeSource#movie.year,
			FilterFun = fun(M, Title, Year) ->
							(M#movie.title =:= Title) andalso (M#movie.year =:= Year)
						end,
			FilteredResults = [M || M <- Results, FilterFun(M, ReferenceTitle, ReferenceYear)],

			Ratings = [M#movie.rating || M <- FilteredResults, M#movie.rating > 0],
		    AverageRating = filmo_utils:round_rating(filmo_utils:average(Ratings)),
		    Verdict = get_verdict_for(AverageRating),

		    %% Heroku doesn't like images from OMDB for some reason, so default to the second source
		    Poster = if
				    	length(FilteredResults) =< 1 ->
				    		"/img/no_result.jpg";
				    	true ->
				    		SecondResult = lists:nth(2, FilteredResults),
				    		SecondResult#movie.poster
				     end,
		    
		    ConvertedResults = [{struct, filmo_utils:movie_to_proplist(Movie)} || Movie <- FilteredResults],
		    CombinedResult = {struct, [{"averageRating", AverageRating}, 
		    						   {"verdict", Verdict},
		    						   {"poster", Poster},
		    						   {"ratings", {array, ConvertedResults}}]},
		    mochijson:encode(CombinedResult)
	end.

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
