%%%-------------------------------------------------------------------
%% @doc my-auth-mysql public API
%% @end
%%%-------------------------------------------------------------------

-module(my_auth_mysql_app).

-behaviour(application).

-include("my_auth_mysql.hrl").
-include_lib("emqx/include/logger.hrl").

-emqx_plugin(?MODULE).

-import(my_auth_mysql_cli, [parse_query/1]).

%% Application callback
-export([start/2
  , stop/1
  , prep_stop/1
]).

%%--------------------------------------------------------------------
%% Application callbacks
%%--------------------------------------------------------------------

start(_StartType, _StartArgs) ->
  ?LOG(warning, "App start...~n"),
  {ok, Sup} = my_auth_mysql_sup:start_link(),
  if_enabled(auth_query, fun load_auth_hook/1),
  if_enabled(acl_query, fun load_acl_hook/1),
  {ok, Sup}.

prep_stop(State) ->
  ?LOG(warning, "App pre_stop...~p~n", [State]),
  emqx:unhook('client.authenticate', fun my_auth_mysql:check/3),
  emqx:unhook('client.check_acl', fun my_acl_mysql:check_acl/5),
  State.

stop(_State) ->
  ?LOG(warning, "App stop...~n"),
  ok.

load_auth_hook(AuthQuery) ->
  ok = my_auth_mysql:register_metrics(),
  SuperQuery = parse_query(application:get_env(?APP, super_query, undefined)),
  {ok, HashType} = application:get_env(?APP, password_hash),
  Params = #{
    auth_query => AuthQuery,
    super_query => SuperQuery,
    hash_type => HashType
  },
  emqx:hook('client.authenticate', fun my_auth_mysql:check/3, [Params]).

load_acl_hook(AclQuery) ->
  ok = my_acl_mysql:register_metrics(),
  emqx:hook('client.check_acl', fun my_acl_mysql:check_acl/5, [#{acl_query => AclQuery}]).


%%--------------------------------------------------------------------
%% Internal function
%%--------------------------------------------------------------------

if_enabled(Cfg, Fun) ->
  case application:get_env(?APP, Cfg) of
    {ok, Query} -> Fun(parse_query(Query));
    undefined -> ok
  end.