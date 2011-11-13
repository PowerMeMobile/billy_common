-module(billy_common_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    billy_common_sup:start_link().

stop(_State) ->
    ok.
