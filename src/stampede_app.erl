-module(stampede_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	application:start(mnesia),
	stutil:init(),
	st_jqs:init(),
    stampede_sup:start_link().

stop(_State) ->
    ok.
