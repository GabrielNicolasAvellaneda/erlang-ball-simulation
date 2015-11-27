-module(ball).
-compile(export_all).

-include_lib("wx/include/wx.hrl").
-include_lib("wx/src/wxe.hrl").

-include_lib("eunit/include/eunit.hrl").

-define(GAME_PROCESS_INSTANCE, game_instance).
-define(UI_PROCESS_INSTANCE, ui_instance).

-define(DEFAULT_BALL_SIZE, 30).
-define(DEFAULT_BALL_X, 30).
-define(DEFAULT_BALL_Y, 30).
-define(DEFAULT_BOUNDS_WIDTH, 320).
-define(DEFAULT_BOUNDS_HEIGHT, 240).

-type position() :: {integer(), integer()}. 
-type velocity() :: {integer(), integer()}.
-type bounds() :: {integer(), integer(), integer(), integer()}.
-record(ball, {position :: position(), size :: integer(), velocity = {0, 0} :: velocity()}).
-record(game_state, {ball :: #ball{}, bounds :: bounds()}).

radius_from_ball_size(Size) -> Size div 2.

generate_ball_geometry(Ball) ->
	{X, Y} = get_position(Ball),
	Size = get_size(Ball),
	Radius = radius_from_ball_size(Size),
	create_circle(X+Radius, Y+Radius, Radius).

create_circle(X, Y, Radius) ->
	{circle, X, Y, Radius}.

create_rectangle(X, Y, Width, Height) ->
	{rectangle, X, Y, Width, Height}.

create_position(X, Y) -> {X, Y}.  

create_velocity(XComponent, YComponent) -> {XComponent, YComponent}.

create_ball(X, Y, Size) -> 
	#ball{position = create_position(X, Y), size = Size, velocity = create_velocity(1,1)}. 

create_bounds(Width, Height) -> {0, 0, Width, Height}.	

create_game_state() ->
	Ball = create_ball(?DEFAULT_BALL_X, ?DEFAULT_BALL_Y, ?DEFAULT_BALL_SIZE),
	Velocity = create_velocity(1, 1),
	BallWithVelocity = set_velocity(Velocity, Ball),
	Bounds = create_bounds(?DEFAULT_BOUNDS_WIDTH, ?DEFAULT_BOUNDS_HEIGHT),
	create_game_state(BallWithVelocity, Bounds).

create_game_state(Ball, Bounds) ->
	#game_state{ball = Ball, bounds = Bounds}.

start() ->
	GamePid = spawn(fun init/0),
	register(?GAME_PROCESS_INSTANCE, GamePid).

init() ->
	GameState = create_game_state(),
	loop(GameState).

%% Ball record manipulation.
get_size(#ball{size = Size}) -> Size.

set_size(Size, Ball = #ball{}) -> Ball#ball{size = Size}.

get_position(#ball{position = Position}) -> Position.

set_position(Position, Ball = #ball{}) -> Ball#ball{position = Position}. 

get_velocity(#ball{velocity = Velocity}) -> Velocity.

set_velocity(Velocity, Ball = #ball{}) -> Ball#ball{velocity = Velocity}.

set_ball_x_position(X, Ball = #ball{}) -> 
	{_, Y} = get_position(Ball),
	set_position({X, Y}, Ball).

set_ball_y_position(Y, Ball = #ball{}) ->
	{X, _} = get_position(Ball),
	set_position({X, Y}, Ball).

%% Game state manipuation.
get_ball(#game_state{ball = Ball}) -> Ball.

set_ball(Ball, GameState = #game_state{}) -> GameState#game_state{ball = Ball}.

get_bounds(#game_state{bounds = Bounds}) -> Bounds.

set_bounds(Bounds, GameState) -> GameState#game_state{bounds = Bounds}.

calculate_next_ball_position(Ball) ->
	{XPosition, YPosition} = get_position(Ball),
	{XVelocity, YVelocity} = get_velocity(Ball),
	{XPosition + XVelocity, YPosition + YVelocity}.

calculate_ball_bounds(Ball) ->
	{X, Y} = get_position(Ball),
	Size = get_size(Ball),
	{X, Y, X+Size, Y+Size}.

is_inside_bounds({X0, Y0, X1, Y1}, {LimitX0, LimitY0, LimitX1, LimitY1}) ->
	(X0 >= LimitX0) andalso (X1 =< LimitX1) andalso (Y0 >= LimitY0) andalso (Y1 =< LimitY1).

update_ball_position_and_velocity(Ball, Bounds) ->
	Position = calculate_next_ball_position(Ball),
	NewBall = set_position(Position, Ball), 
	NewBounds = calculate_ball_bounds(NewBall),
	case is_inside_bounds(NewBounds, Bounds) of
		true -> NewBall;
		false -> fix_ball_out_of_bounds(NewBall, Bounds) 
	end.

reverse_ball_velocity_x_component(Ball) ->
	Velocity = get_velocity(Ball),
	UpdatedVelocity = reverse_velocity_x_component(Velocity),
	set_velocity(UpdatedVelocity, Ball).

reverse_ball_velocity_y_component(Ball) ->
	Velocity = get_velocity(Ball),
	UpdatedVelocity = reverse_velocity_y_component(Velocity),
	set_velocity(UpdatedVelocity, Ball).

reverse_velocity_x_component({X, Y}) -> {-X, Y}.

reverse_velocity_y_component({X, Y}) -> {X, -Y}.

%% @todo: DRY
fix_ball_out_of_bounds_up(Ball, {_LimitX0, LimitY0, _LimitX1, _LimitY1}) ->
	{_, Y0, _, _} = calculate_ball_bounds(Ball),
	case Y0 < LimitY0 of
		true -> UpdatedBallPosition = set_ball_y_position(LimitY0, Ball),
		      	UpdatedBallVelocity = reverse_ball_velocity_y_component(UpdatedBallPosition),
			UpdatedBallVelocity;
		false -> Ball
	end.

fix_ball_out_of_bounds_left(Ball, {LimitX0, _LimitY0, _LimitX1, _LimitY1}) ->
	{X0, _, _, _} = calculate_ball_bounds(Ball),
	case X0 < LimitX0 of
		true -> UpdatedBallPosition = set_ball_x_position(LimitX0, Ball),
		      	UpdatedBallVelocity = reverse_ball_velocity_x_component(UpdatedBallPosition),
			UpdatedBallVelocity;
		false -> Ball
	end.

fix_ball_out_of_bounds_right(Ball, {_LimitX0, _LimitY0, LimitX1, _LimitY1}) ->
	{_, _, X1, _} = calculate_ball_bounds(Ball),
	case X1 > LimitX1 of
		true -> UpdatedBallPosition = set_ball_x_position(LimitX1-get_size(Ball), Ball),
		      	UpdatedBallVelocity = reverse_ball_velocity_x_component(UpdatedBallPosition),
			UpdatedBallVelocity;
		false -> Ball
	end.

fix_ball_out_of_bounds_down(Ball, {_LimitX0, _LimitY0, _LimitX1, LimitY1}) ->
	{_, _, _, Y1} = calculate_ball_bounds(Ball),
	case Y1 > LimitY1 of
		true -> UpdatedBallPosition = set_ball_y_position(LimitY1-get_size(Ball), Ball),
		      	UpdatedBallVelocity = reverse_ball_velocity_y_component(UpdatedBallPosition),
			UpdatedBallVelocity;
		false -> Ball
	end.

fix_ball_out_of_bounds(Ball, Bounds) ->
	UpdatedBall1 = fix_ball_out_of_bounds_right(Ball, Bounds),
	UpdatedBall2 = fix_ball_out_of_bounds_down(UpdatedBall1, Bounds),
	UpdatedBall3 = fix_ball_out_of_bounds_left(UpdatedBall2, Bounds),
	UpdatedBall4 = fix_ball_out_of_bounds_up(UpdatedBall3, Bounds),
	UpdatedBall4.

update_game_state(GameState) ->
	Ball = get_ball(GameState),
	Bounds = get_bounds(GameState),
	UpdatedBall = update_ball_position_and_velocity(Ball, Bounds),
	UpdatedGameState = set_ball(UpdatedBall, GameState),
	UpdatedGameState.

ui_draw_geometry(DC, {circle, X, Y, Radius}) ->
	wxPaintDC:drawCircle(DC, {X, Y}, Radius);
ui_draw_geometry(DC, {rectangle, X, Y, Width, Height}) ->
	wxPaintDC:drawRectangle(DC, {X, Y}, {Width, Height}).

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

bounds_to_rectangle({X0, Y0, X1, Y1}) ->
	{rectangle, X0, Y0, X1, Y1}.  

paint(#wx{obj=Obj}, _WxRef) ->
	DC = wxPaintDC:new(Obj),
	?GAME_PROCESS_INSTANCE ! {self(), get_state},
	receive
		{_From, State} -> 
			Ball = get_ball(State),
			BallGeometry = generate_ball_geometry(Ball),
			BoundsRectangle = bounds_to_rectangle(get_bounds(State)),
			ui_draw_geometry(DC, BoundsRectangle),
			ui_draw_geometry(DC, BallGeometry)
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

%should_update_ball_position_when_out_of_bounds_test() ->
%	BallSize = ?DEFAULT_BALL_SIZE,
%	Ball = create_ball(120, 0, BallSize),
%	Bounds = create_bounds(0, 0, 100, 100),
%	GameState = create_game_state(Ball), 
%	UpdatedGameState = set_bounds(Bounds, GameState),
%	FinalGameState = fix_ball_out_of_bounds_right(UpdatedGameState),
%	Position = get_position(Ball), 
%	?assertEqual({100-BallSize div 2, 0}, Position).
	
