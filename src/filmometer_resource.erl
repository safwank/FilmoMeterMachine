%% @author author <author@example.com>
%% @copyright YYYY author.
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
    EncodedTitle = mochiweb_util:urlencode([{"t", Title}]),
    RequestUri = string:concat("http://www.omdbapi.com/?", EncodedTitle),
    {ok, {{Version, 200, ReasonPhrase}, Headers, Body}} = httpc:request(get, {RequestUri, []}, [], []),
    {Body, ReqData, Context}.