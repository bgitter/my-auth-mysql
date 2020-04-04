%%%-------------------------------------------------------------------
%%% @author zxb
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. 4月 2020 下午1:31
%%%-------------------------------------------------------------------
-module(test).
-author("zxb").

%% API
-export([parse_query/1, topic/1]).

parse_query(undefined) ->
  undefined;
parse_query(Sql) ->
  case re:run(Sql, "'%[ucCad]'", [global, {capture, all, list}]) of
    {match, Variables} ->
      io:format("Variables: ~p~n", [Variables]),
      io:format("Sql: ~p~n", [Sql]),
      Params = [Var || [Var] <- Variables],
      {re:replace(Sql, "'%[ucCad]'", "?", [global, {return, list}]), Params};
    nomatch ->
      {Sql, []}
  end.

topic(<<"eq ", Topic/binary>>) ->
  io:format("binary topic: ~p~n", [Topic]),
  io:format("binary: ~p~n", [binary]),
  {eq, Topic};
topic(Topic) ->
  io:format("topic: ~p~n", [Topic]),
  Topic.