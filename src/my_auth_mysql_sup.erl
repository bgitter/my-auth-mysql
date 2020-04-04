%%%-------------------------------------------------------------------
%% @doc my-auth-mysql top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(my_auth_mysql_sup).

-behaviour(supervisor).

-include("my_auth_mysql.hrl").

-export([start_link/0])
.
%% Supervisor callbacks
-export([init/1]).

start_link() ->
  io:format("Supervisor start_link exec...\n"),
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
  {ok, Server} = application:get_env(?APP, server),
  io:format("Supervisor init exec, Server Info: ~p~n", [Server]),
  PoolSpec = ecpool:pool_spec(?APP, ?APP, my_auth_mysql_cli, Server),
  {ok, {{one_for_one, 10, 100}, [PoolSpec]}}.
