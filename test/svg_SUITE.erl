%%%-------------------------------------------------------------------
%%% @author Dan Gudmundsson <dgud@erlang.org>
%%% @copyright (C) 2017, Dan Gudmundsson
%%% @doc Test it
%%%
%%% @end
%%% Created : 28 Sep 2017 by Dan Gudmundsson <dgud@erlang.org>
%%%-------------------------------------------------------------------
-module(svg_SUITE).

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([show_all/1]).

init_per_suite(Config) -> Config.
end_per_suite(_) -> ok.


all() ->
    [show_all].

show_all(Config) ->
    DataDir = proplists:get_value(data_dir, Config),

    Files = filelib:wildcard(filename:join(DataDir,"*.svg")),
    [svg_wx:start(File) || File <- Files],
    ok.



