%% @author Safwan Kamarrudin <shaihulud@alumni.cmu.edu>
%% @copyright 2012 Safwan Kamarrudin.
%% @doc HTTP helper functions.

-module(http_utils).
-export([wait_for_response/1]).

wait_for_response(RequestId) ->
  receive
    {http, {RequestId, Result}} -> Result
  after
  5000 -> timeout
  end.