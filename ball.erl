-module(ball).
-compile(export_all).

-include_lib("wx/include/wx.hrl").
-include_lib("wx/src/wxe.hrl").

-define(GAME_PROCESS_INSTANCE, game_instance).
-define(UI_PROCESS_INSTANCE, ui_instance).

%% Use a process to manage game state.
%% Use a process to manage ui.
%% The ui process should get updates from the game process with the data to draw.
%% Use another process to update game object position in auto refresh mode.

start() ->
	GamePid = spawn(fun init/0),
	register(?GAME_PROCESS_INSTANCE, GamePid).

init() ->
	Rectangle = {10, 10, 100, 100},
	loop(Rectangle).

update_width(Fun, {X, Y, Width, Height}) ->
	{X, Y, Fun(Width), Height}.	

update_height(Fun, {X, Y, Width, Height}) ->
	{X, Y, Width, Fun(Height)}.

add1(X) -> X + 1.

update_rectangle(Rectangle) ->
	update_height(fun add1/1, update_width(fun add1/1, Rectangle)).

loop(State) ->
	UpdatedState = receive
		{From, get_state} ->
			io:format("get_state called from : ~p~n", [From]), 	       
			From ! {self(), State},
			State
		after 30 ->
			update_rectangle(State)
		end,
	loop(UpdatedState).

paint(Wx = #wx{obj=Obj}, WxRef) ->
	DC = wxPaintDC:new(Obj),
	?GAME_PROCESS_INSTANCE ! {self(), get_state},
	receive
		{From, Rectangle} -> io:format("Received ~p~n", [Rectangle]),
		wxPaintDC:drawRectangle(DC, Rectangle)
	end.

run() ->
	Wx = wx:new(),
	Frame = wxFrame:new(Wx, -1, "Ball simulation"),
	wxFrame:show(Frame),
	OnPaint = fun paint/2,
	wxFrame:connect(Frame, paint, [{callback, OnPaint}]), 
	game_loop(Frame).

game_loop(Frame) ->
	timer:sleep(30),
	repaint(Frame),
	game_loop(Frame).

repaint(Frame) ->
	wxFrame:refresh(Frame).

