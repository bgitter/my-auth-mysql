%%%-------------------------------------------------------------------
%%% @author zxb
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. 4月 2020 下午2:06
%%%-------------------------------------------------------------------
-module(my_acl_mysql).
-author("zxb").

-include("my_auth_mysql.hrl").

-include_lib("emqx/include/emqx.hrl").
-include_lib("emqx/include/logger.hrl").

%% ACL Callbacks
-export([register_metrics/0
  , check_acl/5
  , reload_acl/1
  , description/0]).

-spec(register_metrics() -> ok).
register_metrics() ->
  ?LOG(warning, "Module: [~p] Method: [~p]~n", ["my_acl_mysql", "register_metrics"]),
  lists:foreach(fun emqx_metrics:new/1, ?ACL_METRICS).

check_acl(ClientInfo, PubSub, Topic, NoMatchAction, State) ->
  ?LOG(warning, "Module: [~p] Method: [~p]~n", ["my_acl_mysql", "check_acl"]),
  case do_check_acl(ClientInfo, PubSub, Topic, NoMatchAction, State) of
    ok -> emqx_metrics:inc(?ACL_METRICS(ignore)), ok;
    {stop, allow} -> emqx_metrics:inc(?ACL_METRICS(allow)), {stop, allow};
    {stop, deny} -> emqx_metrics:inc(?ACL_METRICS(deny)), {stop, deny}
  end.

do_check_acl(#{username := <<$$, _/binary>>}, _PubSub, _Topic, _NoMatchAction, _State) ->
  ok;
do_check_acl(ClientInfo, PubSub, Topic, _NoMatchAction, #{acl_query := {AclSql, AclParams}}) ->
  case my_auth_mysql_cli:query(AclSql, AclParams, ClientInfo) of
    {ok, _Columns, []} -> ok;
    {ok, _Columns, Rows} ->
      Rules = filter(PubSub, compile(Rows)),
      case match(ClientInfo, Topic, Rules) of
        {matched, allow} -> {stop, allow};
        {matched, deny} -> {stop, deny};
        nomatch -> ok
      end;
    {error, Reason} ->
      ?LOG(error, "[MySQL] do_check_acl error: ~p~n", [Reason]),
      ok
  end.

match(_ClientInfo, _Topic, []) ->
  nomatch;
match(ClientInfo, Topic, [Rule | Rules]) ->
  case emqx_access_rule:match(ClientInfo, Topic, Rule) of
    nomatch ->
      match(ClientInfo, Topic, Rules);
    {matched, AllowDeny} ->
      {matched, AllowDeny}
  end.

filter(PubSub, Rules) ->
  [Term || Term = {_, _, Access, _} <- Rules, Access =:= PubSub orelse Access =:= pubsub].

compile(Rows) ->
  compile(Rows, []).

compile([], Acc) ->
  Acc;
compile([[Allow, IpAddr, Username, ClientId, Access, Topic] | T], Acc) ->
  Who = who(IpAddr, Username, ClientId),
  Term = {allow(Allow), Who, access(Access), [topic(Topic)]},
  compile(T, [emqx_access_rule:compile(Term) | Acc]).

who(_, <<"$all">>, _) ->
  all;
who(null, null, null) ->
  throw(undefined_who);
who(CIDR, Username, ClientId) ->
  Cols = [{ipaddr, b2l(CIDR)}, {user, Username}, {client, ClientId}],
  case [{C, V} || {C, V} <- Cols, not empty(V)] of
    [Who] -> Who;
    Conds -> {'and', Conds}
  end.

allow(1) -> allow;
allow(0) -> deny;
allow(<<"1">>) -> allow;
allow(<<"0">>) -> deny.

access(1) -> subscribe;
access(2) -> publish;
access(3) -> pubsub;
access(<<"1">>) -> subscribe;
access(<<"2">>) -> publish;
access(<<3>>) -> pubsub.

topic(<<"eq ", Topic/binary>>) ->
  {eq, Topic};
topic(Topic) ->
  Topic.

reload_acl(_State) ->
  ok.

description() ->
  "ACL with Mysql".

b2l(null) -> null;
b2l(B) -> binary_to_list(B).

empty(null) -> true;
empty("") -> true;
empty(<<>>) -> true;
empty(_) -> false.