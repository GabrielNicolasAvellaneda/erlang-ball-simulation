-module(ball).
-compile(export_all).

-include_lib("wx/include/wx.hrl").
-include_lib("wx/src/wxe.hrl").

paint(Wx = #wx{obj=Obj}, WxRef) ->
	DC = wxPaintDC:new(Obj),
	wxPaintDC:drawRectangle(DC, {10, 10, 100, 100}),
	io:format("Received paint event ~p ~p~p~n", [Wx, WxRef, DC]).

run() ->
	Wx = wx:new(),
	Frame = wxFrame:new(Wx, -1, "Ball simulation"),
	wxFrame:show(Frame),
	OnPaint = fun paint/2,
	wxFrame:connect(Frame, paint, [{callback, OnPaint}]), 
	game_loop(Frame).

game_loop(Frame) ->
	timer:sleep(30),
	wxFrame:refresh(Frame),
	game_loop(Frame).
