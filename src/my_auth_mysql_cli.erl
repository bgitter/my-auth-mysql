%%%-------------------------------------------------------------------
%%% @author zxb
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. 4月 2020 下午5:24
%%%-------------------------------------------------------------------
-module(my_auth_mysql_cli).
-author("zxb").

-behaviour(ecpool_worker).

-include("my_auth_mysql.hrl").
-include_lib("emqx/include/emqx.hrl").

%% API
-export([parse_query/1
  , connect/1
  , query/3
]).

%%--------------------------------------------------------------------
%% Avoid SQL Injection: Parse SQL to Parameter Query.
%%--------------------------------------------------------------------

parse_query(undefined) ->
  undefined;
parse_query(Sql) ->
  case re:run(Sql, "'%[ucCad]'", [global, {capture, all, list}]) of
    {match, Variables} ->
      Params = [Var || [Var] <- Variables],
      {re:replace(Sql, "'%[ucCad]'", "?", [global, {return, list}]), Params};
    {nomatch} ->
      {Sql, []}
  end.

%%--------------------------------------------------------------------
%% MySQL Connect/Query
%%--------------------------------------------------------------------

connect(Options) ->
  io:format("Mysql connect start..."),
  io:format("Mysql connect Options: ~p~n", [Options]),
  mysql:start_link(Options).

query(Sql, Params, ClientInfo) ->
  io:format("Sql: ~p~n", [Sql]),
  io:format("Params: ~p~n", [Params]),
  io:format("ClientInfo: ~p~n", [ClientInfo]),
  ecpool:with_client(?APP, fun(C) -> mysql:query(C, Sql, replvar(Params, ClientInfo)) end).

replvar(Params, ClientInfo) ->
  replvar(Params, ClientInfo, []).

replvar([], _ClientInfo, Acc) ->
  lists:reverse(Acc);

replvar(["'%u'" | Params], ClientInfo = #{username := Username}, Acc) ->
  replvar(Params, ClientInfo, [Username | Acc]);

replvar(["'%c'" | Params], ClientInfo = #{clientid := ClientId}, Acc) ->
  replvar(Params, ClientInfo, [ClientId | Acc]);

replvar(["'%a'" | Params], ClientInfo = #{peername := {IpAddr, _}}, Acc) ->
  replvar(Params, ClientInfo, [inet_parse:ntoa(IpAddr) | Acc]);

replvar(["'%C'" | Params], ClientInfo, Acc) ->
  replvar(Params, ClientInfo, [safe_get(cn, ClientInfo) | Acc]);

replvar(["'%d'" | Params], ClientInfo, Acc) ->
  replvar(Params, ClientInfo, [safe_get(dn, ClientInfo) | Acc]);

replvar([Param | Params], ClientInfo, Acc) ->
  replvar(Params, ClientInfo, [Param | Acc]).

safe_get(K, ClientInfo) ->
  bin(maps:get(K, ClientInfo, undefined)).

bin(A) when is_atom(A) -> atom_to_binary(A, utf8);
bin(B) when is_binary(B) -> B;
bin(X) -> X.