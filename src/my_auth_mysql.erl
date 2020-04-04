-module(my_auth_mysql).

-include("my_auth_mysql.hrl").

-include_lib("emqx/include/emqx.hrl").
-include_lib("emqx/include/logger.hrl").
-include_lib("emqx/include/types.hrl").

-export([register_metrics/0
  , check/3,
  description/0
]).

-define(EMPTY(Username), (Username =:= undefined orelse Username =:= <<>>)).

-spec(register_metrics() -> ok).
register_metrics() ->
  lists:foreach(fun emqx_metrics:new/1, ?AUTH_METRICS).

check(ClientInfo = #{password := Password}, AuthResult,
    #{auth_query := {AuthSql, AuthParams},
      super_query := SuperQuery,
      hash_type := HashType}) ->
  CheckPass = case my_auth_mysql_cli:query(AuthSql, AuthParams, ClientInfo) of
                {ok, [<<"password">>], [[PassHash]]} ->
                  check_pass({PassHash, Password}, HashType);
                {ok, [<<"password">>, <<"salt">>], [[PassHash, Salt]]} ->
                  check_pass({PassHash, Salt, Password}, HashType);
                {ok, _Columns, []} ->
                  {error, not_found};
                {error, Reason} ->
                  ?LOG(error, "[MySQL] query '~p' failed: ~p", [AuthSql, Reason]),
                  {error, not_found}
              end,
  case CheckPass of
    ok ->
      emqx_metrics:inc(?AUTH_METRICS(success)),
      {stop, AuthResult#{is_superuser => is_superuser(SuperQuery, ClientInfo),
        anonymous => false,
        auth_result => success}};
    {error, not_found} ->
      emqx_metrics:inc(?AUTH_METRICS(ignore)),
      ok;
    {error, ResultCode} ->
      ?LOG(error, "[MySQL] Auth from mysql failed: ~p", [ResultCode]),
      emqx_metrics:inc(?AUTH_METRICS(failure)),
      {stop, AuthResult#{auth_result => ResultCode, anonymous => false}}
  end.


%%--------------------------------------------------------------------
%% Is Superuser?
%%--------------------------------------------------------------------

-spec(is_superuser(maybe({string(), list()}), emqx_types:client()) -> boolean()).
is_superuser(undefined, _ClientInfo) -> false;
is_superuser({SuperSql, Params}, ClientInfo) ->
  case my_auth_mysql_cli:query(SuperSql, Params, ClientInfo) of
    {ok, [_Super], [[1]]} -> true;
    {ok, [_Super], [[_False]]} -> false;
    {ok, [_Super], []} -> false;
    {error, [_Error]} -> false
  end.

check_pass(Password, HashType) ->
  case emqx_passwd:check_pass(Password, HashType) of
    ok -> ok;
    {error, _Reason} -> {error, not_authorized}
  end.

description() -> "Authentication with MySQL".