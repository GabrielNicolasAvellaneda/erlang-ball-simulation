-module(ball).
-compile(export_all).

-include_lib("wx/include/wx.hrl").
-include_lib("wx/src/wxe.hrl").

-define(GAME_PROCESS_INSTANCE, game_instance).
-define(UI_PROCESS_INSTANCE, ui_instance).

-record(velocity, {x = 1 :: number(), y = 1 :: number()}).
-record(position, {x :: number(), y :: number()}).
-record(ball, {position :: #position{}, geometry :: tuple(), velocity = #velocity{} :: #velocity{}}).

%% Use a process to manage game state.
%% Use a process to manage ui.
%% The ui process should get updates from the game process with the data to draw.
%% Use another process to update game object position in auto refresh mode.

create_circle(X, Y, Radius) ->
	{circle, X, Y, Radius}.

create_velocity(VelX, VelY) -> #velocity{x = VelX, y = VelY}.

create_ball(X, Y, Radius) -> #ball{geometry = create_circle(X, Y, Radius)}. 

start() ->
	GamePid = spawn(fun init/0),
	register(?GAME_PROCESS_INSTANCE, GamePid).

init() ->
	Object = create_ball(0, 0, 30),
	loop(Object).

update_width(Fun, {X, Y, Width, Height}) ->
	{X, Y, Fun(Width), Height}.

update_height(Fun, {X, Y, Width, Height}) ->
	{X, Y, Width, Fun(Height)}.

get_geometry(#ball{geometry = Geometry }) -> Geometry.

set_geometry(Geometry, Ball = #ball{}) -> Ball#ball{geometry = Geometry}.

get_position(#ball{position = Position}) -> Position.

set_position(Position, Ball = #ball{}) -> Ball#ball{position = Position}. 

get_velocity(#ball{velocity = Velocity}) -> Velocity.

set_velocity(Velocity, Ball = #ball{}) -> Ball#ball{velocity = Velocity}.

add1(X) -> X + 1.

update_geometry_position({circle, X, Y, Radius}, Velocity = #velocity{x = VelX, y = VelY}) ->
	{circle, X + VelX, Y + VelY, Radius}.

move_x(Fun, {circle, X, Y, Radius}) ->
	{circle, Fun(X), Y, Radius}.

move_y(Fun, {circle, X, Y, Radius}) ->
	{circle, X, Fun(Y), Radius}.

update_object(Ball = #ball{}) ->
	Geometry = get_geometry(Ball),
	Velocity = get_velocity(Ball),
	UpdatedGeometry = update_geometry_position(Geometry, Velocity),
	set_geometry(UpdatedGeometry, Ball).	

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
		{From, State} -> 
			Geometry = get_geometry(State),
			ui_draw(DC, Geometry)
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

