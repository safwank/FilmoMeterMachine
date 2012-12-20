%% @author Safwan Kamarrudin <shaihulud@alumni.cmu.edu>
%% @copyright 2012 Safwan Kamarrudin.

%% @doc Callbacks for the filmometer application.

-module(filmometer_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for filmometer.
start(_Type, _StartArgs) ->
    filmometer_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for filmometer.
stop(_State) ->
    ok.
