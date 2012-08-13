-module(st_session).

% -behaviour(gen_server).

% % API
% -export([start_link/3]).

% % gen_server callbacks
% -export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% % State record
% -record(state, {server_socket = undefined, routing_rules = [], worker_count = 0, 
% 					max_workers = infinity, min_workers = 1, worker_options = []}).

%% ===================================================================
%% API functions
%% ===================================================================

