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
    {{_Version, 200, _ReasonPhrase}, _Headers, Body} = OMDBResult,
    {Body, ReqData, Context}.

get_omdb_result(Title) ->
	EncodedTitle = mochiweb_util:urlencode([{"t", Title}]),
    RequestUri = string:concat("http://www.omdbapi.com/?", EncodedTitle),
    {ok, RequestId} = httpc:request(get, {RequestUri, []}, [], [{sync, false}]),
    wait_for_response(RequestId).

 wait_for_response(RequestId) ->
 	receive 
 		{http, {RequestId, Result}} -> Result 
 	after 
 		5000 -> timeout 
 	end.