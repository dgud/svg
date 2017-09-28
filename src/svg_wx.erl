%%%-------------------------------------------------------------------
%%% @author Dan Gudmundsson <dgud@erlang.org>
%%% @copyright (C) 2017, Dan Gudmundsson
%%% @doc Draw svg vector graphics images with wx
%%%
%%% @end
%%% Created :  4 Jul 2017 by Dan Gudmundsson <dgud@erlang.org>
%%%-------------------------------------------------------------------
-module(svg_wx).

-export([start/1]).

-export([start/0]). %% debug

-include_lib("wx/include/wx.hrl").

start() ->
    Dir = code:lib_dir(svg),
    start(filename:join([Dir, "test", "main_wxlogo.svg"])).

start(File) ->
    io:format("Import: ~p~n",[File]),
    %% Start wx application
    _ = wx:new(),
    %% Create the OS window (called frame)
    NoParent = wx:null(),         % This is the top level window, we have no parent
    NoId = ?wxID_ANY,             % We can set an id for every widget,
                                  % we don't need a specific id here
    Frame = wxFrame:new(NoParent, NoId, "Hello Joe"),

    %% Create a widget as background and drawing area belonging to the frame.
    Panel = wxPanel:new(Frame),
    White = {255,255,255},
    wxWindow:setBackgroundColour(Panel, White),

    %% Create a sizer, the sizer handles widgets placement and size
    Sizer = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(Sizer, Panel, [{proportion, 1}, {flag, ?wxEXPAND}]),
    wxFrame:setSizer(Frame, Sizer),

    {ok, SvgOrig} = svg:import(File),
    SvgWx = setup_redraw(SvgOrig, self()),

    %% Listen to events
    wxPanel:connect(Panel, paint, [{callback, fun(Ev, _) -> redraw(Ev, SvgWx) end}]),
    wxFrame:connect(Frame, close_window, [{skip, true}]),
    %%      {skip, true} will let the event be handled by the default handler
    %%      which will close the window

    %% Show the frame
    wxFrame:show(Frame),
    loop(),
    ok.

loop() ->
    receive
        #wx{event=#wxClose{}} ->
            ok;
        Msg ->
            io:format("~p:~p: Got ~p~n",[?MODULE, ?FUNCTION_NAME, Msg]),
            loop()
    end.

%% This is called in a spawned process, so don't deadlock by
%% calling the process above while that waits in calls to wx.

redraw(#wx{obj=Panel}, SvgWx) ->
    %% Create a drawing context on the Panel
    DC = wxPaintDC:new(Panel),
    GC = wxGraphicsContext:create(DC),
    wxGraphicsContext:setBrush(GC, ?wxNullBrush),
    wxGraphicsContext:setPen(GC, ?wxNullPen),

    SvgWx(Panel, GC),

    %% Nothing is drawn until you destroy your drawing contexts
    wxGraphicsObject:destroy(GC),
    wxPaintDC:destroy(DC),
    ok.

setup_redraw(#{groups:=Gs}=Data, Parent) ->
    Defs = maps:get(defs, Data, #{}),
    Width = maps:get(width, Data, 0),
    Height = maps:get(height, Data, 0),
    {X0,Y0,W,H} = maps:get(viewBox, Data, {0,0,Width,Height}),
    %% io:format("Draw ~p ~p~n",[{X0,Y0},{W,H}]),
    fun(Panel, GC) ->
            try
                {PW,PH} = wxPanel:getSize(Panel),
                wxGraphicsContext = wx:getObjectType(GC), %% Assert
                case X0 =:= 0 andalso Y0 =:= 0 of
                    false -> wxGraphicsContext:translate(GC, -X0+(W/2),-Y0 + (H/2));
                    true -> ignore
                end,
                Scale = min(PW/W, PH/H),
                wxGraphicsContext:scale(GC, Scale, Scale),
                do_wx(Gs, Defs#{path=>[], pens=>#{}, brushes=>#{}}, GC)
            catch _:_Reason ->
                    St = erlang:get_stacktrace(),
                    exit(Parent, {{redraw_crash,_Reason}, St})
            end
    end.

do_wx([#{g:=Gs}=Cmd|R], #{path:=Path}=Defs0, GC) ->
    case maps:get(style, Cmd, "") of
        #{display:="none"} ->
            do_wx(R, Defs0, GC);
        _ ->
            Matrix1 = transform(Cmd, GC),
            Defs1 = Defs0#{path:=[Cmd|Path]},
            Defs = do_wx(Gs, Defs1, GC),
            reset_matrix(Matrix1, GC),
            do_wx(R, Defs#{path:=Path}, GC)
    end;
do_wx([Cmd|R], Defs0, GC) ->
    case maps:get(style, Cmd, false) of
        #{display:="none"} ->
            do_wx(R, Defs0, GC);
        _ ->
            Matrix = transform(Cmd, GC),
            Defs = setup_colors(Cmd, Defs0, GC),
            GCP = wxGraphicsContext:createPath(GC),
            do_wx_2(Cmd, GCP),
            wxGraphicsContext:drawPath(GC, GCP),
            wxGraphicsObject:destroy(GCP),
            reset_matrix(Matrix, GC),
            do_wx(R, Defs, GC)
    end;
do_wx([], Defs, _GCD) ->
    Defs.

do_wx_2(#{type:=path, d:=Path}, GC) ->
    lists:foldl(fun(Cmd, {Pos, Prev}) ->
                        Next = do_wx_path(Cmd, Pos, Prev, GC),
                        {Next, Cmd}
                end, {{0,0}, undefined}, Path);
do_wx_2(#{type:=rect, x:=X0, y:=Y0, height:=H0, width:=W0}=Rect, GCP) ->
    Rx0 = maps:get(rx, Rect, 0.0),
    wxGraphicsPath:addRoundedRectangle(GCP, X0, Y0, W0, H0, Rx0);
do_wx_2(#{type:=ellipse, rx:=Rx, ry:=Ry}=Cmd, GCP) ->
    %% io:format("Cmd: ~p~n",[Cmd]),
    Cx = maps:get(cx, Cmd, 0),
    Cy = maps:get(cy, Cmd, 0),
    wxGraphicsPath:addEllipse(GCP, Cx-Rx, Cy-Ry, Rx*2, Ry*2);
do_wx_2(#{type:=circle, r:=R0}=Cmd, GCP) ->
    Cx = maps:get(cx, Cmd, 0),
    Cy = maps:get(cy, Cmd, 0),
    wxGraphicsPath:addCircle(GCP, Cx, Cy, R0);
do_wx_2(#{type:=Type}=_Cmd, _GC) ->
    io:format("~p NYI: ~p ~n~p~n",[?FUNCTION_NAME, Type, _Cmd]).

do_wx_path({{move, AR}, XY}, Pos0, _Prev, GCP) ->
    Pos = move(AR, XY, Pos0),
    wxGraphicsPath:moveToPoint(GCP, Pos),
    Pos;
do_wx_path({{curveto, AR}, {XY1, XY2, XY3}}, Pos0, _Prev, GCP) ->
    Pos1 = move(AR, XY1, Pos0),
    Pos2 = move(AR, XY2, Pos0),
    Pos3 = move(AR, XY3, Pos0),
    wxGraphicsPath:addCurveToPoint(GCP, Pos1, Pos2, Pos3),
    Pos3;
do_wx_path({{smooth_curveto, AR}, {XY2, XY3}}, Pos0, Prev, GCP) ->
    Pos1 = smooth_curveto(Pos0, Prev),
    Pos2 = move(AR, XY2, Pos0),
    Pos3 = move(AR, XY3, Pos0),
    wxGraphicsPath:addCurveToPoint(GCP, Pos1, Pos2, Pos3),
    Pos3;
do_wx_path({{lineto, AR}, XY}, Pos0, _Prev, GCP) ->
    Next = move(AR, XY, Pos0),
    wxGraphicsPath:addLineToPoint(GCP, Next),
    Next;
do_wx_path({{v_lineto, AR}, Y}, {X0,Y0}, _Prev, GCP) ->
    Next = move(AR, {X0,Y}, {0,Y0}),
    wxGraphicsPath:addLineToPoint(GCP, Next),
    Next;
do_wx_path({{h_lineto, AR}, X}, {X0,Y0}, _Prev, GCP) ->
    Next = move(AR, {X,Y0}, {X0,0}),
    wxGraphicsPath:addLineToPoint(GCP, Next),
    Next;
do_wx_path({{arc, AR}, {RadXY0, XRot, Fa, Fs, XY0}}, Pos0, _Prev, GCP) ->
    %% see https://mortoray.com/2017/02/16/rendering-an-svg-elliptical-arc-as-bezier-curves/
    XY = move(AR, XY0, Pos0),
    {R0, C0, {Start,Delta}} = endpointToCenterArc(Pos0, XY, RadXY0, XRot, Fa, Fs),
    Points = arc2bezier(C0, R0, XRot, Start, Delta),
    P2 = lists:foldl(fun({Q1,Q2,P2}, _P1) ->
                             wxGraphicsPath:addCurveToPoint(GCP, Q1, Q2, P2),
                             P2
                     end, Pos0, Points),
    P2;

do_wx_path({{stop, _},_}, _Pos0, _Prev, GCP) ->
    wxGraphicsPath:closeSubpath(GCP),
    {0,0};
do_wx_path({{Cmd,_}, _Args}, Pos, _Prev, _GCP) ->
    io:format("~p NYI: ~p~n",[?FUNCTION_NAME, Cmd]),
    Pos.

smooth_curveto({X3,Y3}, {{curveto, _AR}, {_XY1, {X2,Y2}, _}}) ->
    {-(X3-X2), -(Y3-Y2)};
smooth_curveto({X3,Y3}, {{smooth_curveto, _AR}, {{X2,Y2}, _}}) ->
    {-(X3-X2), -(Y3-Y2)};
smooth_curveto(Pos0, _) ->
    Pos0.

move(abs, Abs, _) -> Abs;
move(rel, Rel, Orig) -> add(Rel,Orig).

add({X,Y}, {X0,Y0}) -> {X+X0,Y+Y0}.
sub({X,Y}, {X0,Y0}) -> {X-X0,Y-Y0}.

transform(#{transform:={A,B,C,D,Tx,Ty}}, GC) ->
    Orig = wxGraphicsContext:getTransform(GC),
    M = wxGraphicsContext:createMatrix(GC, [{a,A}, {b,B}, {c,C},
                                            {d,D}, {tx,Tx}, {ty,Ty}]),
    wxGraphicsContext:concatTransform(GC, M),
    Orig;
transform(_, _) ->
    none.


arc2bezier(Center, R, XA, Start, Delta) ->
    End = Start+Delta,
    Sign = case End < Start of
               true -> -1;
               false -> 1
           end,
    Step0 = math:pi()/4,
    Prev = arc2point(Center, R, XA, Start),
    arc2bezier(abs(End-Start), Step0, Sign, Prev, Center, R, XA, Start).

arc2bezier(Remain, Step, Sign, P1, Center, R, XA, Start) ->
    SignStep = Sign*min(Remain, Step),
    Next = Start+SignStep,
    P2 = arc2point(Center, R, XA, Next),
    AlphaT = math:tan(SignStep/2),
    Alpha = math:sin(SignStep)*(math:sqrt(4+3*AlphaT*AlphaT)-1)/3,
    Q1 = add(P1, arc2deri(Alpha, R, XA, Start)),
    Q2 = sub(P2, arc2deri(Alpha, R, XA, Next)),
    B = {Q1,Q2,P2},
    %% io:format(" ~.3f ~p ~.3f ~p ~n",[Start, P1, Next, P2]),
    case Remain < Step of
        true -> [B];
        false -> [B|arc2bezier(Remain-Step, Step, Sign, P2, Center, R, XA, Next)]
    end.

arc2point({CX,CY}, {RX,RY}, XA, T) ->
    {CX+RX*math:cos(XA)*math:cos(T)-RY*math:sin(XA)*math:sin(T),
     CY+RX*math:sin(XA)*math:cos(T)+RY*math:cos(XA)*math:sin(T)}.

arc2deri(Alpha, {RX,RY}, XA, T) ->
    {Alpha*(-RX * math:cos(XA) * math:sin(T) - RY * math:sin(XA) * math:cos(T)),
     Alpha*(-RX * math:sin(XA) * math:sin(T) + RY * math:cos(XA) * math:cos(T))}.

endpointToCenterArc({P1X,P1Y}, {P2X,P2Y}, {RX00,RY00}, XA, Fa, Fs) ->
    RX0 = abs(RX00),    RY0 = abs(RY00),
    DX2 = (P1X-P2X)/2, DY2 = (P1Y-P2Y)/2,
    X1P = math:cos(XA)*DX2+math:sin(XA)*DY2,
    Y1P = -math:sin(XA)*DX2+math:cos(XA)*DY2,
    Rxs0 = RX0*RX0, Rys0 = RY0*RY0,
    X1PS = X1P*X1P, Y1PS = Y1P*Y1P,
    CR = X1PS/Rxs0 + Y1PS/Rys0,
    {RX,RY} = case CR > 1 of
                  true ->
                      S = math:sqrt(CR),
                      {S*RX0, S*RY0};
                  false -> {RX0, RY0}
              end,
    RXS = RX*RX, RYS = RY*RY,
    DQ = RXS * Y1PS + RYS * X1PS,
    PQ = (RXS*RYS - DQ) / DQ,
    Q = case Fa =:= Fs of
            true -> -math:sqrt(max(0, PQ));
            false -> math:sqrt(max(0, PQ))
        end,
    CXP =  Q*RX*Y1P/RY,
    CYP = -Q*RY*X1P/RX,
    CX = math:cos(XA)*CXP - math:sin(XA)*CYP + (P1X + P2X)/2,
    CY = math:sin(XA)*CXP + math:cos(XA)*CYP + (P1Y + P2Y)/2,
    Theta = svgAngle( 1, 0, (X1P-CXP)/RX, (Y1P-CYP)/RY),
    %% (F.6.5.6)
    Delta0 = svgAngle((X1P - CXP)/RX, (Y1P - CYP)/RY,
                      (-X1P - CXP)/RX, (-Y1P-CYP)/RY),

    %% Delta = Math.Mod(delta, Math.PIf * 2 );
    Delta = if not Fs -> Delta0 - 2*math:pi();
               true -> Delta0
            end,
    {{RX,RY}, {CX,CY}, {Theta, Delta}}.

svgAngle(UX,UY,VX,VY) ->
    Len = math:sqrt(UX*UX+UY*UY) * math:sqrt(VX*VX+VY*VY),
    Dot = UX*VX+UY*VY,
    case (UX*VY-UY*VX) < 0 of
        true -> -math:acos(Dot/Len);
        false -> math:acos(Dot/Len)
    end.

reset_matrix(none, _) -> ok;
reset_matrix(M, GC) -> wxGraphicsContext:setTransform(GC, M).

setup_colors(#{style:=Style}, #{path:=Path}=Defs0, GC) ->
    Defs1 = setup_pen([Style|Path], Defs0, GC),
    setup_brush([Style|Path], Defs1, GC);
setup_colors(Cmd, #{path:=Path}=Defs0, GC) ->
    Defs1 = setup_pen([Cmd|Path], Defs0, GC),
    setup_brush([Cmd|Path], Defs1, GC).

setup_pen(Data, Defs0, GC) ->
    case mapfind(stroke, Data, "none") of
        "none" ->
            wxGraphicsContext:setPen(GC, ?wxNullPen),
            Defs0#{active_pen=>none};
        Stroke ->
            Active = maps:get(active_pen, Defs0, undefined),
            Opacity = mapfind(opacity, Data, 1.0),
            StrOpacity = mapfind('stroke-opacity', Data, 1.0),
            Width = mapfind('stroke-width', Data, 2.0),
            case make_pen_key(Stroke, StrOpacity*Opacity, Width) of
                Active -> Defs0;
                PenKey ->
                    {Pen, Defs} = setup_pen2(PenKey, Defs0),
                    wxGraphicsContext:setPen(GC, Pen),
                    Defs#{active_pen=>PenKey}
                end
    end.

setup_pen2(Key, #{pens:=Pens}=Defs) ->
    case maps:get(Key, Pens, undefined) of
        undefined ->
            Pen = make_pen(Key, Defs),
            {Pen, Defs#{pens=>Pens#{Key=>Pen}}};
        Pen -> {Pen, Defs}
    end.

make_pen_key(Col, A, W0) when is_float(W0) ->
    make_pen_key(Col, A, round(W0));
make_pen_key(Col, A, {px,W0}) ->
    make_pen_key(Col, A, W0);
make_pen_key({R,G,B}, A, W) ->
    {{R,G,B, round(A*255)}, W};
make_pen_key({url, _}=Url, A, W) ->
    {Url, A, W}.

make_pen({{_,_,_,_} = RGB, W}, _Defs) ->
    %% io:format("Pen: ~p~n",[RGB]),
    wxPen:new(RGB, [{width,W}]);
make_pen({{url,[_|_]=Ref}, A, W}, Defs) ->
    Gs = maps:get(gradients, Defs),
    #{'xlink:href':=Url} = maps:get(Ref, Gs),
    #{stop:=List} = _Pen = maps:get(string:trim(Url, leading, "#"), Gs),
    L = [rgba(RGB, maps:get('stop-opacity', Style, A))
         || #{style:=#{'stop-color':=RGB}=Style} <- List],
    RGB = average(L),
    %% io:format("Pen ref: ~p~n",[_Pen]),
    wxPen:new(RGB, [{width,W}]).

setup_brush(Data, Defs0, GC) ->
    case mapfind(fill, Data, "none") of
        "none" ->
            wxGraphicsContext:setBrush(GC, ?wxNullBrush),
            Defs0#{active_brush=>none};
        Fill ->
            Active = maps:get(active_brush, Defs0, undefined),
            Opacity = mapfind(opacity, Data, 1.0),
            FillOpacity = mapfind('fill-opacity', Data, 1.0),
            case make_brush_key(Fill, FillOpacity*Opacity) of
                Active -> Defs0;
                BrushKey ->
                    {Brush, Defs} = setup_brush2(BrushKey, Defs0, GC),
                    wxGraphicsContext:setBrush(GC, Brush),
                    Defs#{active_brush=>BrushKey}
            end
    end.

make_brush_key({R,G,B}, Alpha) ->
    %% Todo add size, opaqueness and style..
    {R,G,B,trunc(Alpha*255)};
make_brush_key({url, _}=BrushKey,Alpha) ->
    {BrushKey, Alpha}.

setup_brush2(Key, #{brushes:=Brushes}=Defs, GC) ->
    case maps:get(Key, Brushes, undefined) of
        undefined ->
            Brush = make_brush(Key, Defs, GC),
            {Brush, Defs#{brushes=>Brushes#{Key=>Brush}}};
        Brush -> {Brush, Defs}
    end.

make_brush({_,_,_,_}=RGB, _, _) ->
    %% io:format("Brush: ~p~n",[RGB]),
    wxBrush:new(RGB);
make_brush({{url, Ref},Alpha}, #{gradients:=Gs}, GC) ->
    case maps:get(Ref, Gs) of
        #{type:=linear_grad, 'xlink:href':=Url} = Data ->
            #{x1:=X1, y1:=Y1, x2:=X2, y2:=Y2} = Data,
            #{stop:=List} = _B = maps:get(string:trim(Url, leading, "#"), Gs),
            {C1,C2} = pick2(List, Alpha),
            % io:format("Brush: ~p ~p~n => ~p ~p~n~n", [_B, Data,C1,C2]),
            T = maps:get(transform, Data, svg:m_identity()),
            {Xs,Ys} = svg:m_mul_point({X1,Y1}, T),
            {Xe,Ye} = svg:m_mul_point({X2,Y2}, T),
            wxGraphicsContext:createLinearGradientBrush(GC,Xs,Ys,Xe,Ye,C1,C2);
        #{type:=radial_grad, 'xlink:href':=Url} = Data ->
            #{cx:=CX0, cy:=CY0, fx:=X1, fy:=Y1, r:=R0} = Data,
            #{stop:=List} = _B = maps:get(string:trim(Url, leading, "#"), Gs),
            {C1,C2} = pick2(List,Alpha),
            %% io:format("Brush: ~p ~p ~n", [_B, Data]),
            T = maps:get(transform, Data, svg:m_identity()),
            {Xs,Ys} = svg:m_mul_point({X1,Y1}, T),
            {CX,CY} = svg:m_mul_point({CX0,CY0}, T),
            {R, _} = svg:m_mul_vec({R0,0}, T),
            wxGraphicsContext:createRadialGradientBrush(GC,Xs,Ys,CX,CY,R,C1,C2)
    end.

average(L) ->
    Len = length(L),
    {R,G,B,A} = average(L, {0,0,0,0}),
    {R div Len, G div Len, B div Len, A div Len}.

average([{R0,G0,B0,A0}|T], {R,G,B,A}) ->
    average(T, {R0+R,G0+G,B0+B,A0+A});
average([], RGB) ->
    RGB.

pick2(List, DefA) ->
    [First|T] = lists:sort(fun(#{offset:=A},#{offset:=B}) -> A =< B end, List),
    {pick_c(First,DefA), pick_c(lists:last(T),DefA)}.

pick_c(#{style:=C}, DefA) ->
    #{'stop-color':=RGB} = C,
    A = maps:get('stop-opacity', C, DefA),
    rgba(RGB, A).

rgba({R,G,B}, A) ->
    {R,G,B,round(A*255)}.

mapfind(Key, [H|T], Def) ->
    case H of
        #{Key:=Value} -> Value;
        _ -> mapfind(Key,T, Def)
    end;
mapfind(_, [], Def) ->
    Def.
