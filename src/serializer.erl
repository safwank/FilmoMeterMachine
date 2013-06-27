%% @author Safwan Kamarrudin <shaihulud@alumni.cmu.edu>
%% @copyright 2012 Safwan Kamarrudin.
%% @doc Content serializer and deserializer.

-module(serializer).
-export([serialize/2, deserialize/2]).

-include_lib("webmachine/include/webmachine.hrl").

serialize(Content, Type) ->
  case Type of
    json -> mochijson:encode(Content);
    _ -> mochijson:encode(Content)
  end.

deserialize(Content, Type) ->
  case Type of
    json -> mochijson:decode(Content);
    _ -> mochijson:decode(Content)
  end.


