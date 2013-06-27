%% @author Safwan Kamarrudin <shaihulud@alumni.cmu.edu>
%% @copyright 2012 Safwan Kamarrudin.
%% @doc TMDB source.

-module(tmdb_source).
-export([get_result/2]).

-include("movie.hrl").

get_result(Criteria, Pid) ->
  APIKey = "8abd8211399f1196bdefef458fc4c5ed",

  SearchTitle = proplists:get_value("title", Criteria),
  EncodedTitle = mochiweb_util:urlencode([{"query", SearchTitle}]),
  SearchYear = proplists:get_value("year", Criteria),

  BaseUri = lists:flatten(
    io_lib:format("http://api.themoviedb.org/3/search/movie?api_key=~s&~s",
      [APIKey, EncodedTitle])),
  RequestUri = case SearchYear of
    undefined -> BaseUri;
    _ -> BaseUri ++ "&year=" ++ SearchYear
  end,
  {ok, Result} = httpc:request(get, {RequestUri, [{"Accept", "application/json"}]}, [], [{sync, true}]),

  {{_Version, 200, _ReasonPhrase}, _Headers, Body} = Result,
  ParsedJsonResult = serializer:deserialize(Body, json),
  {struct, [_Page, {_Results, {array, Results}}, _TotalPages, _TotalResults]} = ParsedJsonResult,
  Pid ! build_result_from(Results).

build_result_from(Results) ->
  PosterBaseUri = "http://cf2.imgobject.com/t/p/w185/",
  Movies =
    [#movie{source = "TMDB", title = Title,
    year = case ReleaseDate of
      null -> 0;
      ReleaseDate ->
        case string:to_integer(string:substr(ReleaseDate, 1, 4)) of
          {error, _} -> 0;
          {Year, _} -> Year
        end
    end,
    actors = "",
    poster = case Poster of
      null -> "";
      _ -> string:concat(PosterBaseUri, Poster)
    end,
    rating = Rating}
      || {struct, [_Adult, _Backdrop, _Id, _OriginalTitle,
      {_, ReleaseDate}, {_, Poster}, _Popularity,
      {_, Title}, {_, Rating}, _Votes]}
      <- Results],
  Movies.