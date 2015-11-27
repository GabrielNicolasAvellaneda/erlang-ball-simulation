-module(ball).
-compile(export_all).

-include_lib("wx/include/wx.hrl").
-include_lib("wx/src/wxe.hrl").

-define(GAME_PROCESS_INSTANCE, game_instance).
-define(UI_PROCESS_INSTANCE, ui_instance).

-record(velocity, {x = 2 :: number(), y = 2 :: number()}).
-record(position, {x :: number(), y :: number()}).
-record(ball, {position :: #position{}, geometry :: tuple(), velocity = #velocity{} :: #velocity{}}).
-record(game_state, {ball = #ball{}, bounds = {0, 0, 400, 400}}).

%% Use a process to manage game state.
%% Use a process to manage ui.
%% The ui process should get updates from the game process with the data to draw.
%% Use another process to update game object position in auto refresh mode.

create_circle(X, Y, Radius) ->
	{circle, X, Y, Radius}.

create_velocity(VelX, VelY) -> #velocity{x = VelX, y = VelY}.

create_ball(X, Y, Radius) -> #ball{geometry = create_circle(X, Y, Radius)}. 

create_game_state() ->
	#game_state{ball = create_ball(0, 0, 30)}.

start() ->
	GamePid = spawn(fun init/0),
	register(?GAME_PROCESS_INSTANCE, GamePid).

init() ->
	GameState = create_game_state(),
	loop(GameState).

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

get_ball(#game_state{ball = Ball}) -> Ball.

set_ball(Ball, GameState) -> GameState#game_state{ball = Ball}.

get_bounds(#game_state{bounds = Bounds}) -> Bounds.

set_bounds(Bounds, GameState) -> GameState#game_state{bounds = Bounds}.

add1(X) -> X + 1.

update_geometry_position({circle, X, Y, Radius}, Velocity = #velocity{x = VelX, y = VelY}) ->
	{circle, X + VelX, Y + VelY, Radius}.

set_geometry_x_coordinate(NewX, {circle, _X, Y, Radius}) -> {circle, NewX, Y, Radius}.
set_geometry_y_coordinate(NewY, {circle, X, _Y, Radius}) ->  {circle, X, NewY, Radius}.

move_x(Fun, {circle, X, Y, Radius}) ->
	{circle, Fun(X), Y, Radius}.

move_y(Fun, {circle, X, Y, Radius}) ->
	{circle, X, Fun(Y), Radius}.

update_object(Ball = #ball{}) ->
	Geometry = get_geometry(Ball),
	Velocity = get_velocity(Ball),
	UpdatedGeometry = update_geometry_position(Geometry, Velocity),
	set_geometry(UpdatedGeometry, Ball).	

reverse_x_component(Velocity = #velocity{x = X}) ->
	Velocity#velocity{x = -X}.

reverse_y_component(Velocity = #velocity{y = Y}) ->
	Velocity#velocity{y = -Y}.

fix_out_of_bounds_left(GameState) ->
	{BoundsX0, BoundsY0, BoundsX1, BoundsY1} = get_bounds(GameState), 
	{BallX0, BallY0, BallX1, BallY1} = get_ball_bounds(get_ball(GameState)), 
	Ball = get_ball(GameState),
	if
		BallX0 < BoundsX0 ->
			Velocity = get_velocity(Ball),
			UpdatedVelocity = reverse_x_component(Velocity),
			UpdatedBall = set_velocity(UpdatedVelocity, Ball),
			Geometry = get_geometry(UpdatedBall),
			UpdatedGeometry = set_geometry_x_coordinate(BoundsX0+15, Geometry),
			set_ball(set_geometry(UpdatedGeometry, UpdatedBall), GameState);
		true -> GameState
	end.

fix_out_of_bounds_right(GameState) ->
	{BoundsX0, BoundsY0, BoundsX1, BoundsY1} = get_bounds(GameState), 
	{BallX0, BallY0, BallX1, BallY1} = get_ball_bounds(get_ball(GameState)), 
	Ball = get_ball(GameState),
	if
		BallX1 > BoundsX1 ->
			Velocity = get_velocity(Ball),
			UpdatedVelocity = reverse_x_component(Velocity),
			UpdatedBall = set_velocity(UpdatedVelocity, Ball),
			Geometry = get_geometry(UpdatedBall),
			UpdatedGeometry = set_geometry_x_coordinate(BoundsX1 - 15, Geometry),
			set_ball(set_geometry(UpdatedGeometry, UpdatedBall), GameState);
		true -> GameState
	end.

fix_out_of_bounds_up(GameState) ->
	GameState.

fix_out_of_bounds_down(GameState) ->
	GameState.

fix_out_of_bounds(GameState) ->
	UpdatedState1 = fix_out_of_bounds_left(GameState),
	UpdatedState2 = fix_out_of_bounds_up(UpdatedState1),
        UpdatedState3 = fix_out_of_bounds_right(UpdatedState2),
	UpdatedState4 = fix_out_of_bounds_down(UpdatedState3).

get_ball_bounds(Ball) ->
	{circle, X, Y, Radius} = get_geometry(Ball),
	{X - Radius, Y - Radius, X + Radius, Y + Radius}.

update_game_state(GameState) ->
	Ball = get_ball(GameState),
	UpdatedBall = update_object(Ball),
	UpdatedGameState = set_ball(UpdatedBall, GameState),
	fix_out_of_bounds(UpdatedGameState).

ui_draw(DC, {circle, X, Y, Radius}) ->
	wxPaintDC:drawCircle(DC, {X, Y}, Radius). 

% @todo: A client should register to get game state updates. Invert update control.
loop(State) ->
	UpdatedState = receive
		{From, get_state} ->
			From ! {self(), State},
			State
		after 30 ->
			update_game_state(State)
			%State
		end,
	loop(UpdatedState).

paint(Wx = #wx{obj=Obj}, WxRef) ->
	DC = wxPaintDC:new(Obj),
	?GAME_PROCESS_INSTANCE ! {self(), get_state},
	receive
		{From, State} -> 
			Ball = get_ball(State),
			Geometry = get_geometry(Ball),
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

