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
	Object = {circle, 10, 10, 30},
	loop(Object).

update_width(Fun, {X, Y, Width, Height}) ->
	{X, Y, Fun(Width), Height}.	

update_height(Fun, {X, Y, Width, Height}) ->
	{X, Y, Width, Fun(Height)}.

add1(X) -> X + 1.

move_x(Fun, {circle, X, Y, Radius}) ->
	{circle, Fun(X), Y, Radius}.

move_y(Fun, {circle, X, Y, Radius}) ->
	{circle, X, Fun(Y), Radius}.

update_object({circle, X, Y, Radius}) ->
	move_x(fun add1/1, move_y(fun add1/1, {circle, X, Y, Radius})).

ui_draw(DC, {circle, X, Y, Radius}) ->
	wxPaintDC:drawCircle(DC, {X, Y}, Radius). 

% @todo: A client should register to get game state updates.
loop(State) ->
	UpdatedState = receive
		{From, get_state} ->
			From ! {self(), State},
			State
		after 30 ->
			update_object(State)
		end,
	loop(UpdatedState).

paint(Wx = #wx{obj=Obj}, WxRef) ->
	DC = wxPaintDC:new(Obj),
	?GAME_PROCESS_INSTANCE ! {self(), get_state},
	receive
		{From, State} -> ui_draw(DC, State)
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

