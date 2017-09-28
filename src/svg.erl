%%%-------------------------------------------------------------------
%%% @author Dan Gudmundsson <dgud@erlang.org>
%%% @copyright (C) 2017, Dan Gudmundsson
%%% @doc Import svg vector graphics format
%%%
%%% @end
%%% Created :  4 Jul 2017 by Dan Gudmundsson <dgud@erlang.org>
%%%-------------------------------------------------------------------
-module(svg).
-export([import/1]).

% Matrix ops
-export([m_identity/0, m_matrix/6, m_translate/2, m_scale/2,
         m_rotate/1, m_skew_x/1, m_skew_y/1,
         m_multiply/1, m_multiply/2,
         m_mul_point/2, m_mul_vec/2
        ]).


%% internals
-export([top/3, parse/3, svg/4, defs/4, gradient/4, ignored/4]).

import(File) ->
    EF = {event_fun, fun top/3},
    ES = {event_state, #{stack=>[], groups=>[]}},
    case xmerl_sax_parser:file(File, [EF,ES]) of
        {ok, #{svg:=Svg}, <<>>} ->
	    {ok, Svg};
        {error, _} = Err ->
            Err;
        {Error, {_,_,Line}, Reason, _ET, _St} ->
            io:format("~s:~p: ERROR: ~p:~p~n", [File, Line, Error, Reason]),
            {Error, Reason}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Xml stuff

top(startDocument, _, State) -> State;
top(endDocument, _, State) -> State;
top({comment, _}, _, State) -> State;
top({startPrefixMapping,_,_}, _, State) -> State;
top({endPrefixMapping,_}, _, State) -> State;
top({ignorableWhitespace, _}, _, State) -> State;
top({startElement, _, _, _, _}=Ev, Loc, State) ->
    parse(Ev, Loc, State);
top({endElement, _, _, _}=Ev, Loc, State) ->
    parse(Ev, Loc, State);
top({characters, _}=Ev, Loc, State) ->
    parse(Ev, Loc, State);
top(Ev, Loc, State) ->
    unhandled(Ev, Loc, State),
    State.

parse(Ev, Loc, #{stack:=[_|_]}=State) ->
    invoke(Ev, Loc, State);
parse({startElement, _, "svg", _, As}, _Loc, #{stack:=[]}=State) ->
    push({svg, attrs(As)}, State);
parse(Ev, Loc, Es) ->
    unhandled(Ev, Loc, Es),
    Es.

svg({startElement, _, "defs", _, As}, _Loc, _Data, Es) ->
    push({defs, attrs(As)}, Es#{defs=>[]});
svg({startElement, _, "g", _, As}, _Loc, Data, Es) ->
    GStack = maps:get(g_stack, Data, []),
    replace(Data#{g_stack=>[attrs(As, #{g=>[]})|GStack]}, Es);
svg({endElement, _, "g", _}, _Loc,
    #{g_stack:=[#{g:=Gs0}=Group0|GStack0]}=Data0, #{groups:=TGs}=Es) ->
    Group = parse_extra(Group0#{g:=lists:reverse(Gs0)}),
    case GStack0 of
        %% Recursive groups
        [#{g:=Gs}=Group1|GStack] ->
            replace(Data0#{g_stack:=[Group1#{g:=[Group|Gs]}|GStack]}, Es);
        [] ->
            replace(Data0#{g_stack:=[]}, Es#{groups:=[Group|TGs]})
    end;

svg({startElement, _, "path", _, As}, _Loc, Data, Es) ->
    add_to_group(parse_path(attrs(As, #{type=>path})), Data, Es);
svg({startElement, _, "rect", _, As}, _Loc, Data, Es) ->
    add_to_group(parse_extra(attrs(As, #{type=>rect})), Data, Es);
svg({startElement, _, "circle", _, As}, _Loc, Data, Es) ->
    add_to_group(parse_extra(attrs(As, #{type=>circle})), Data, Es);
svg({startElement, _, "ellipse", _, As}, _Loc, Data, Es) ->
    add_to_group(parse_extra(attrs(As, #{type=>ellipse})), Data, Es);
svg({startElement, _, "line", _, As}, _Loc, Data, Es) ->
    add_to_group(parse_extra(attrs(As, #{type=>line})), Data, Es);
svg({startElement, _, "polyline", _, As0}, _Loc, Data, Es) ->
    #{points:=Ps0} = As = attrs(As0, #{type=>polyline}),
    Ps = [XY || {_, XY} <- parse_cmds(Ps0, $l, [])],
    add_to_group(parse_extra(As#{points:=Ps}), Data, Es);
svg({startElement, _, "polygon", _, As0}, _Loc, Data, Es) ->
    #{points:=Ps0} = As = attrs(As0, #{type=>polygon}),
    Ps = [XY || {_, XY} <- parse_cmds(Ps0, $l, [])],
    add_to_group(parse_extra(As#{points:=Ps}), Data, Es);

svg({startElement, _, "namedview"=What, _, _As}, _Loc, _Data, Es) ->
    push({ignored, What}, Es);
svg({startElement, _, "metadata"=What, _, _As}, _Loc, _Data, Es) ->
    push({ignored, What}, Es);
svg({endElement, _, "svg", _}, _Loc, Data, #{groups:=Gs}=Es) ->
    Defs = maps:get(defs, Es, #{}),
    pop(Es#{svg=>Data#{groups=>lists:reverse(Gs), defs=>Defs}});
svg({endElement, _, _, _}, _Loc, _Data, Es) ->
    Es;
svg(Ev, Loc, _Data, Es) ->
    unhandled(Ev, Loc, Es),
    Es.

add_to_group(What, #{g_stack:=[#{g:=Groups}=Top|Stack]}=Data, Es) ->
    replace(Data#{g_stack:=[Top#{g:=[What|Groups]}|Stack]}, Es);
add_to_group(What, _Data, #{groups:=Gs}=Es) ->
    Es#{groups:=[What|Gs]}.

defs({endElement, _, "defs", _}, _Loc, Data, #{defs:=Defs}=Es) ->
    pop(Es#{defs:=Data#{gradients=>maps:from_list(Defs)}});
defs({startElement, _, "linearGradient", _, As}, _Loc, _Data, Es) ->
    push({gradient, attrs(As, #{type=>linear_grad, stop=>[]})}, Es);
defs({startElement, _, "radialGradient", _, As}, _Loc, _Data, Es) ->
    push({gradient, attrs(As, #{type=>radial_grad, stop=>[]})}, Es);
defs({endElement, _, "linearGradient", _}, _Loc, _Data, Es) ->
    Es;
defs({startElement, _, "perspective"=What, _, _As}, _Loc, _Data, Es) ->
    push({ignored, What}, Es);
defs({startElement, _, "filter"=What, _, _As}, _Loc, _Data, Es) ->
    push({ignored, What}, Es);
defs(Ev, _Loc, _Data, Es) ->
    unhandled(Ev, _Loc, Es),
    Es.

gradient({startElement, _, "stop", _, As}, _Loc, #{stop:=Stop}=Data, Es) ->
    replace(Data#{stop:=[parse_style(attrs(As))|Stop]}, Es);
gradient({endElement, _, "stop", _}, _Loc, _Data, Es) ->
    Es;
gradient({endElement, _, _, _}, _Loc, #{id:=Id}=Data0, #{defs:=Ds}=Es) ->
    Data = case maps:get(stop, Data0, undefined) of
               undefined -> Data0;
               Stop -> Data0#{stop:=lists:reverse(Stop)}
           end,
    pop(Es#{defs:=[{Id, parse_extra(Data)}|Ds]});
gradient(Ev, _Loc, _Data, Es) ->
    unhandled(Ev, _Loc, Es),
    Es.

ignored({endElement, _, State,_}, _Loc, State, Es) ->
    pop(Es);
ignored(_, _Loc, _, Es) ->
    Es.

invoke(Ev, Loc, #{stack:=[Top|_]}=Es) ->
    try
        case Top of
            {State, Data} ->
                ?MODULE:State(Ev, Loc, Data, Es);
            State when is_atom(State) ->
                ?MODULE:State(Ev, Loc, Es)
        end
    catch
        error:function_clause=Reason ->
            case erlang:get_stacktrace() of
                [{?MODULE,Func, _,_}|_] when Func =:= Top ->
                    unhandled(Ev, Loc, Es),
                    Es;
                [{?MODULE,Func, _,_}|_] when Func =:= element(1,Top) ->
                    unhandled(Ev, Loc, Es),
                    Es;
                _ ->
                    io:format("~p:~n ~P~n", [Reason, erlang:get_stacktrace(), 20]),
                    io:format("Last: ~P~n~P~n",[Ev,5,Es,10]),
                    throw({fatal_error, parser_error})
            end;
        error:Reason ->
            io:format("~p:~n ~P~n", [Reason, erlang:get_stacktrace(), 20]),
            io:format("Last: ~P~n~P~n",[Ev,5,Es,10]),
            throw({fatal_error, parser_error})
    end.

%% Helpers

attrs(As) ->
    attrs(As, #{}).

attrs(As, Map0) ->
    Add = fun({_, Ext, Name, Str}, Map) ->
                  KeyStr = case wsp(Ext) of
                               [] -> Name;
                               _ -> wsp(Ext) ++ ":" ++ wsp(Name)
                           end,
                  {Key, Value} = attr(KeyStr, Str),
		  Map#{Key=>Value}
	  end,
    lists:foldl(Add, Map0, As).

attr("fill", Str) -> {fill, attr_color(wsp(Str))};
attr("stroke", Str) -> {stroke, attr_color(wsp(Str))};
attr("stop-color", Str) -> {'stop-color', attr_color(wsp(Str))};
attr("viewBox", Vs0) ->
    {{X,Y},R0} = xy(wsp(Vs0)),
    {{W,H}, _} = xy(comma_wsp(R0)),
    {viewBox, {X,Y,W,H}};
attr(Name, Str) ->
    {list_to_atom(Name), attr_value(wsp(Str))}.

attr_color([$#,R0,R1,G0,G1,B0,B1]) ->
    {list_to_integer([R0,R1],16),list_to_integer([G0,G1],16),list_to_integer([B0,B1],16)};
attr_color([$#,R0,G0,B0]) ->
    {list_to_integer([R0,R0],16),list_to_integer([G0,G0],16),list_to_integer([B0,B0],16)};
attr_color("none") -> "none";
attr_color("url"++UrlInParen) ->
    {url, hd(string:split(string:trim(UrlInParen, both, "(#"),")"))};
attr_color("rgb"++RGBs) ->
    R0 = string:trim(RGBs, both, "()\s\t\n"),
    {R,R1} = get_number(R0),
    {G,R2} = get_number(comma_wsp(R1)),
    {B,_} = get_number(comma_wsp(R2)),
    {R,G,B};
attr_color(String) -> color(String).

attr_value(Val) ->
    case get_number(Val) of
        {error, _} -> Val;
        {Num, []} -> Num;
        {_, [$.|_]} -> Val; %% Versions numbers
        {Num, "px"} -> {px, Num};
        {Num, "%"} ->  {percent, Num};
        {_Num, _Ext} ->
            %io:format("Ignore: Num ~p => ~p ~p ~n",[Val, _Num, _Ext]),
            Val
    end.

push(State,#{stack:=Stack}=Es) -> Es#{stack:=[State|Stack]}.
pop(#{stack:=[_|Stack]}=Es) -> Es#{stack:=Stack}.
replace(Data,#{stack:=[{State,_Data}|Stack]}=Es) -> Es#{stack:=[{State,Data}|Stack]}.

unhandled(Ev, {_,File,Line}, #{stack:=[]}) ->
    io:format("~s:~p ignored:   ~P~n", [File, Line, strip_chars(Ev), 20]),
    ok;
unhandled(Ev, {_,File,Line}, #{stack:=State0}) ->
    State = [fun({S,_}) -> S;(S) -> S end(St) || St <- State0],
    io:format("~s:~p ~p ignored:~n\t ~P~n", [File, Line, State, strip_chars(Ev), 20]),
    ok.

strip_chars({characters, List}=What) ->
    try lists:split(50, List) of
	{First, _} -> {characters, First++"...."}
    catch _:_ -> What end;
strip_chars(Other) -> Other.

%%%

parse_path(#{d:=Cmds}=P) ->
    parse_extra(P#{d:=parse_cmds(Cmds)}).

parse_extra(P) ->
    parse_transform(parse_style(P)).

parse_style(#{style:=Str}=G) ->
    G#{style:=parse_style1(Str)};
parse_style(G) ->
    G.

parse_style1(S0) when is_list(S0) ->
    L = lists:map(fun(StyleStr) ->
                          [Key,Val] = string:lexemes(StyleStr, ":"),
                          attr(wsp(Key),Val)
                  end, string:lexemes(S0, ";")),
    maps:from_list(L).

parse_cmds(CmdString) ->
    parse_cmds(CmdString, undefined, []).

parse_cmds([], _, Acc) ->
    lists:reverse(Acc);
parse_cmds([Op|_]=Str, Op0, Acc) ->
    try parse_cmd(Str,Op0) of
        {Rest, Next, skip} ->
            parse_cmds(Rest, Next, Acc);
        {Rest, Next, Data} ->
            parse_cmds(Rest, Next, [{op(Op), Data}|Acc])
    catch throw:{ErrorStr, Next} ->
            io:format("Parse error: ~ts @ ~p~n",[ErrorStr, Next]),
            io:format("  in: ~ts~n", [Str]),
            error(parse_error)
    end.

parse_cmd([WSP|R0], Op)
  when WSP =:= $\s; WSP =:= $\t; WSP =:= $\n; WSP =:= $\r ->
    {R0, Op, skip};
parse_cmd([Cmd|R0], _) when Cmd =:= $m; Cmd =:= $M ->
    {XY, R1} = xy(R0),
    Next = case Cmd of
               $m -> $l;
               $M -> $M
           end,
    {R1, Next, XY};
parse_cmd([Cmd|R0], _) when Cmd =:= $l; Cmd =:= $L ->
    {XY, R1} = xy(R0),
    {R1, Cmd, XY};
parse_cmd([Cmd|R0], _) when Cmd =:= $v; Cmd =:= $V ->
    {X, R1} = get_number(R0),
    {R1, Cmd, X};
parse_cmd([Cmd|R0], _) when Cmd =:= $h; Cmd =:= $H ->
    {Y, R1} = get_number(R0),
    {R1, Cmd, Y};
parse_cmd([Cmd|R0], _) when Cmd =:= $c; Cmd =:= $C ->
    {XY1, R1} = xy(R0),
    {XY2, R2} = xy(comma_wsp(R1)),
    {XY3, R3} = xy(comma_wsp(R2)),
    {R3, Cmd, {XY1, XY2, XY3}};
parse_cmd([Cmd|R0], _) when Cmd =:= $s; Cmd =:= $S ->
    {XY1, R1} = xy(R0),
    {XY2, R2} = xy(comma_wsp(R1)),
    {R2, Cmd, {XY1, XY2}};
parse_cmd([Cmd|R0], _) when Cmd =:= $q; Cmd =:= $Q ->
    {XY1, R1} = xy(R0),
    {XY2, R2} = xy(comma_wsp(R1)),
    {R2, Cmd, {XY1, XY2}};
parse_cmd([Cmd|R0], _) when Cmd =:= $t; Cmd =:= $T ->
    {XY, R1} = xy(R0),
    {R1, Cmd, XY};
parse_cmd([Cmd|R0], _) when Cmd =:= $z; Cmd =:= $Z ->
    {R0, Cmd, []};
parse_cmd([Cmd|R0], _) when Cmd =:= $a; Cmd =:= $A ->
    {RadXY, R1} = xy(R0),
    {XRot, R2} = get_number(comma_wsp(R1)),
    {LargeArc, R3} = get_number(comma_wsp(R2)),
    {SweepFlag, R4} = get_number(comma_wsp(R3)),
    {XY, R5} = xy(comma_wsp(R4)),
    {R5, Cmd, {RadXY, XRot, LargeArc, SweepFlag, XY}};
parse_cmd(Str, Op) ->
    case get_number(Str) of
        {error, _} -> throw({"Can not handle op:", {hd(Str), Op}});
        _ ->
            {[Op|Str], Op, skip}
    end.

parse_transform(#{transform:="none"}=G) ->
    maps:remove(transform, G);
parse_transform(#{transform:=Str}=G) ->
    Matrix = parse_transform(Str, []),
    G#{transform:=Matrix};
parse_transform(#{gradientTransform:=Str}=G) ->
    Matrix = parse_transform(Str, []),
    maps:remove(gradientTransform, G#{transform=>Matrix});
parse_transform(G) ->
    G.

parse_transform([WSP|R0], Acc)
  when WSP =:= $\s; WSP =:= $\t; WSP =:= $\n; WSP =:= $\r ->
    parse_transform(R0, Acc);
parse_transform("matrix" ++ R0, Acc) ->
    {{A,B}, R1} =  xy(open_par(R0)),
    {{C,D}, R2} =  xy(comma_wsp(R1)),
    {{E,F}, R3} =  xy(comma_wsp(R2)),
    R = close_par(R3),
    parse_transform(R, [m_matrix(A,B,C,D,E,F)|Acc]);
parse_transform("translate"++R0, Acc) ->
    {X, R1} = get_number(wsp(open_par(R0))),
    {Y, R2} = case get_number(comma_wsp(R1)) of
                  {error, _} -> {0, R1};
                  Y0 -> Y0
              end,
    R = close_par(R2),
    parse_transform(R, [m_translate(X,Y)|Acc]);
parse_transform("scale"++R0, Acc) ->
    {X, R1} = get_number(wsp(open_par(R0))),
    {Y, R2} = case get_number(comma_wsp(R1)) of
                  {error, _} -> {X, R1};
                  Y0 -> Y0
              end,
    R = close_par(R2),
    parse_transform(R, [m_scale(X,Y)|Acc]);
parse_transform("skewX"++R0, Acc) ->
    {X, R1} = get_number(wsp(open_par(R0))),
    R = close_par(R1),
    parse_transform(R, [m_skew_x(X/180*math:pi())|Acc]);
parse_transform("skewY"++R0, Acc) ->
    {Y, R1} = get_number(wsp(open_par(R0))),
    R = close_par(R1),
    parse_transform(R, [m_skew_y(Y/180*math:pi())|Acc]);
parse_transform("rotate"++R0, Acc) ->
    {A, R1} = get_number(wsp(open_par(R0))),
    try xy(comma_wsp(R1)) of
        {{X,Y}, R2} ->
            R = close_par(R2),
            M0 = m_translate(-X,-Y),
            MR = m_rotate(A/180*math:pi()),
            M1 = m_translate(X,Y),
            parse_transform(R, [m_multiply([M0,MR,M1])|Acc])
    catch _:_ ->
            R = close_par(R1),
            parse_transform(R, [m_rotate(A/180*math:pi())|Acc])
    end;
parse_transform([], Acc) ->
    m_multiply(lists:reverse(Acc)).

xy(R0) ->
    {X, R1} = get_number(wsp(R0)),
    {Y, R2} = get_number(comma_wsp(R1)),
    true =:= is_list(R2) orelse throw({"Expected an number", R0}),
    {{X,Y}, R2}.

get_number([$.|_]=Str) ->
    string:to_float([$0|Str]);
get_number(Rest) ->
    case string:to_integer(Rest) of
        {_, [$.|_]} -> string:to_float(Rest);
        {Int, [$e|_]=R} -> string:to_float([integer_to_list(Int),".0"|R]);
        {Int, [$E|_]=R} -> string:to_float([integer_to_list(Int),".0"|R]);
        IntRes -> IntRes
    end.

open_par([$(|R]) ->
    R;
open_par(R0) ->
    [$(|R] = wsp(R0),
    R.
close_par([$)|R]) ->
    R;
close_par(R0) ->
    [$)|R] = wsp(R0),
    R.

comma_wsp([$,|R]) -> wsp(R);
comma_wsp(R) ->
    case wsp(R) of
        R -> R;
        Cont -> comma_wsp(Cont)
    end.

wsp([16#20|R]) -> wsp(R); %$\s
wsp([16#9|R])  -> wsp(R); %$\t
wsp([16#D|R])  -> wsp(R); %$\r
wsp([16#A|R])  -> wsp(R); %$\n
wsp(R) -> R.

op($m) -> {move,rel};
op($M) -> {move,abs};
op($l) -> {lineto,rel};
op($L) -> {lineto,abs};
op($v) -> {v_lineto,rel};
op($V) -> {v_lineto,abs};
op($h) -> {h_lineto,rel};
op($H) -> {h_lineto,abs};
op($c) -> {curveto,rel};
op($C) -> {curveto,abs};
op($s) -> {smooth_curveto,rel};
op($S) -> {smooth_curveto,abs};
op($q) -> {qbezier_curveto,rel};
op($Q) -> {qbezier_curveto,abs};
op($t) -> {smooth_qbezier_curveto,rel};
op($T) -> {smooth_qbezier_curveto,abs};
op($z) -> {stop,rel};
op($Z) -> {stop,abs};
op($a) -> {arc, rel};
op($A) -> {arc, abs};
op(Op) -> exit({nyi, Op}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%

color("red") -> {255, 0, 0};
color("green") -> { 0, 128, 0};
color("blue") -> { 0, 0, 255};
color("yellow") -> {255, 255, 0};
color("cyan") -> { 0, 255, 255};
color("magenta") -> {255, 0, 255};
color("black") -> { 0, 0, 0};
color("grey") -> {128, 128, 128};
color("gray") -> {128, 128, 128};
color("white") -> {255, 255, 255};
%% All COLOR KEYWORDS
color("aliceblue") -> {240, 248, 255};
color("antiquewhite") -> {250, 235, 215};
color("aqua") -> { 0, 255, 255};
color("aquamarine") -> {127, 255, 212};
color("azure") -> {240, 255, 255};
color("beige") -> {245, 245, 220};
color("bisque") -> {255, 228, 196};
color("blanchedalmond") -> {255, 235, 205};
color("blueviolet") -> {138, 43, 226};
color("brown") -> {165, 42, 42};
color("burlywood") -> {222, 184, 135};
color("cadetblue") -> { 95, 158, 160};
color("chartreuse") -> {127, 255, 0};
color("chocolate") -> {210, 105, 30};
color("coral") -> {255, 127, 80};
color("cornflowerblue") -> {100, 149, 237};
color("cornsilk") -> {255, 248, 220};
color("crimson") -> {220, 20, 60};
color("darkblue") -> { 0, 0, 139};
color("darkcyan") -> { 0, 139, 139};
color("darkgoldenrod") -> {184, 134, 11};
color("darkgray") -> {169, 169, 169};
color("darkgreen") -> { 0, 100, 0};
color("darkgrey") -> {169, 169, 169};
color("darkkhaki") -> {189, 183, 107};
color("darkmagenta") -> {139, 0, 139};
color("darkolivegreen") -> { 85, 107, 47};
color("darkorange") -> {255, 140, 0};
color("darkorchid") -> {153, 50, 204};
color("darkred") -> {139, 0, 0};
color("darksalmon") -> {233, 150, 122};
color("darkseagreen") -> {143, 188, 143};
color("darkslateblue") -> { 72, 61, 139};
color("darkslategray") -> { 47, 79, 79};
color("darkslategrey") -> { 47, 79, 79};
color("darkturquoise") -> { 0, 206, 209};
color("darkviolet") -> {148, 0, 211};
color("deeppink") -> {255, 20, 147};
color("deepskyblue") -> { 0, 191, 255};
color("dimgray") -> {105, 105, 105};
color("dimgrey") -> {105, 105, 105};
color("dodgerblue") -> { 30, 144, 255};
color("firebrick") -> {178, 34, 34};
color("floralwhite") -> {255, 250, 240};
color("forestgreen") -> { 34, 139, 34};
color("fuchsia") -> {255, 0, 255};
color("gainsboro") -> {220, 220, 220};
color("ghostwhite") -> {248, 248, 255};
color("gold") -> {255, 215, 0};
color("goldenrod") -> {218, 165, 32};
color("greenyellow") -> {173, 255, 47};
color("honeydew") -> {240, 255, 240};
color("hotpink") -> {255, 105, 180};
color("indianred") -> {205, 92, 92};
color("indigo") -> { 75, 0, 130};
color("ivory") -> {255, 255, 240};
color("khaki") -> {240, 230, 140};
color("lavender") -> {230, 230, 250};
color("lavenderblush") -> {255, 240, 245};
color("lawngreen") -> {124, 252, 0};
color("lemonchiffon") -> {255, 250, 205};
color("lightblue") -> {173, 216, 230};
color("lightcoral") -> {240, 128, 128};
color("lightcyan") -> {224, 255, 255};
color("lightgoldenrodyellow") -> {250, 250, 210};
color("lightgray") -> {211, 211, 211};
color("lightgreen") -> {144, 238, 144};
color("lightgrey") -> {211, 211, 211};
color("lightpink") -> {255, 182, 193};
color("lightsalmon") -> {255, 160, 122};
color("lightseagreen") -> { 32, 178, 170};
color("lightskyblue") -> {135, 206, 250};
color("lightslategray") -> {119, 136, 153};
color("lightslategrey") -> {119, 136, 153};
color("lightsteelblue") -> {176, 196, 222};
color("lightyellow") -> {255, 255, 224};
color("lime") -> { 0, 255, 0};
color("limegreen") -> { 50, 205, 50};
color("linen") -> {250, 240, 230};
color("maroon") -> {128, 0, 0};
color("mediumaquamarine") -> {102, 205, 170};
color("mediumblue") -> { 0, 0, 205};
color("mediumorchid") -> {186, 85, 211};
color("mediumpurple") -> {147, 112, 219};
color("mediumseagreen") -> { 60, 179, 113};
color("mediumslateblue") -> {123, 104, 238};
color("mediumspringgreen") -> { 0, 250, 154};
color("mediumturquoise") -> { 72, 209, 204};
color("mediumvioletred") -> {199, 21, 133};
color("midnightblue") -> { 25, 25, 112};
color("mintcream") -> {245, 255, 250};
color("mistyrose") -> {255, 228, 225};
color("moccasin") -> {255, 228, 181};
color("navajowhite") -> {255, 222, 173};
color("navy") -> { 0, 0, 128};
color("oldlace") -> {253, 245, 230};
color("olive") -> {128, 128, 0};
color("olivedrab") -> {107, 142, 35};
color("orange") -> {255, 165, 0};
color("orangered") -> {255, 69, 0};
color("orchid") -> {218, 112, 214};
color("palegoldenrod") -> {238, 232, 170};
color("palegreen") -> {152, 251, 152};
color("paleturquoise") -> {175, 238, 238};
color("palevioletred") -> {219, 112, 147};
color("papayawhip") -> {255, 239, 213};
color("peachpuff") -> {255, 218, 185};
color("peru") -> {205, 133, 63};
color("pink") -> {255, 192, 203};
color("plum") -> {221, 160, 221};
color("powderblue") -> {176, 224, 230};
color("purple") -> {128, 0, 128};
color("rosybrown") -> {188, 143, 143};
color("royalblue") -> { 65, 105, 225};
color("saddlebrown") -> {139, 69, 19};
color("salmon") -> {250, 128, 114};
color("sandybrown") -> {244, 164, 96};
color("seagreen") -> { 46, 139, 87};
color("seashell") -> {255, 245, 238};
color("sienna") -> {160, 82, 45};
color("silver") -> {192, 192, 192};
color("skyblue") -> {135, 206, 235};
color("slateblue") -> {106, 90, 205};
color("slategray") -> {112, 128, 144};
color("slategrey") -> {112, 128, 144};
color("snow") -> {255, 250, 250};
color("springgreen") -> { 0, 255, 127};
color("steelblue") -> { 70, 130, 180};
color("tan") -> {210, 180, 140};
color("teal") -> { 0, 128, 128};
color("thistle") -> {216, 191, 216};
color("tomato") -> {255, 99, 71};
color("turquoise") -> { 64, 224, 208};
color("violet") -> {238, 130, 238};
color("wheat") -> {245, 222, 179};
color("whitesmoke") -> {245, 245, 245};
color("yellowgreen") -> {154, 205, 50}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%

m_identity() ->
    {1,0, 0,1, 0,0}.
m_matrix(A,B,C,D,E,F) ->
    {A,B,C,D,E,F}.
m_translate(X,Y) ->
    {1,0, 0,1, X,Y}.
m_scale(SX,SY) ->
    {SX,0, 0,SY, 0,0}.
m_skew_x(A) ->
    {1,0, math:tan(A),1, 0,0}.
m_skew_y(A) ->
    {1,math:tan(A), 0,1, 0,0}.
m_rotate(A) ->
    CS = math:cos(A), SN = math:sin(A),
    {CS, SN, -SN, CS, 0, 0}.

m_multiply([M]) -> M;
m_multiply([M0,M1|R]) ->
    m_multiply([m_multiply(M1, M0)|R]).

m_multiply({T0,T1,T2,T3,T4,T5},{S0,S1,S2,S3,S4,S5}) ->
    TT0 = T0 * S0 + T1 * S2,
    TT2 = T2 * S0 + T3 * S2,
    TT4 = T4 * S0 + T5 * S2 + S4,

    {TT0, TT0*S1+T1*S3,
     TT2, TT2*S1+T3*S3,
     TT4, TT4*S1+T5*S3+S5}.

m_mul_point({X,Y}, {T0,T1,T2,T3,T4,T5}) ->
    {X*T0 + Y*T2 + T4,
     X*T1 + Y*T3 + T5}.

m_mul_vec({X,Y}, {T0,T1,T2,T3,_T4,_T5}) ->
    {X*T0 + Y*T2, X*T1 + Y*T3}.
