-module(piface2).
-behaviour(gen_server).


-define(is_uint8(T), (((T) band (bnot 16#ff)) =:=  0)).
-define(is_uint16(T), (((T) band (bnot 16#ffff)) =:=  0)).
-define(is_uint32(T), (((T) band (bnot 16#ffffffff)) =:=  0)).

%% api

-export([
	init_interrupt/0
]).

-export([
	gpio_get/1, 
	gpio_set/1, 
	gpio_clr/1
]).


-export([
	read_input/1, 
	read_output/1,
	write_output/2]).

%% gen_server api
-export([
	start_link/0,
	stop/0
]).

%% gen_server callbacks
-export([
	init/1, 
	handle_call/3, 
	handle_cast/2, 
	handle_info/2,
	terminate/2, 
	code_change/3
]).

-define(info(Str),		lager:info("~p",[Str])).
-define(critical(Str),	lager:critical("~p",[Str])).

-define(PIFACE_SRV, 	piface_srv).
-define(SPIDEV,			"spidev0.0").
-define(SPI_BUS, 		0).
-define(SPI_DEVICE, 	0).

-define(TRANSFER_LEN,   3).
-define(TRANSFER_DELAY, 5).
-define(TRANSFER_SPEED, 1000000).
-define(TRANSFER_BPW,   8).

-define(SPI_WRITE_CMD,  16#40).
-define(SPI_READ_CMD,   16#41).

%% Port configuration
-define(IODIRA, 		16#00).    %% I/O direction A
-define(IODIRB, 		16#01).    %% I/O direction B
-define(IOCON,  		16#0A).     %% I/O config
-define(GPIOA,  		16#12).     %% port A
-define(GPIOB,  		16#13).     %% port B
-define(GPPUA,  		16#0C).     %% port A pullups
-define(GPPUB,  		16#0D).     %% port B pullups
-define(OUTPUT_PORT, 	?GPIOA).
-define(INPUT_PORT,  	?GPIOB).
-define(GPINTENA, 		16#04).
-define(GPINTENB, 		16#05).
-define(DEFVALA,  		16#06).
-define(DEFVALB,  		16#07).
-define(INTCONA,  		16#08).
-define(INTCONB,  		16#09).

-define(IOCON_HAEN,   	2#00001000).
-define(IOCON_MIRROR, 	2#01000000).

-record(ctx,{spi,state = init}).

start_link() ->
    gen_server:start_link({local, ?PIFACE_SRV}, ?MODULE, [], []).

stop() ->
    gen_server:call(?PIFACE_SRV, stop).


%%--------------------------------------------------------------------
init_interrupt() ->
    spi_write(?INTCONB,  16#00), %% interrupt on any change
    spi_write(?GPINTENB, 16#FF), %% enable interrupts on B
    ok.

%%--------------------------------------------------------------------
gpio_get(Pin) when ?is_uint8(Pin) ->
    Bits = read_input(),
    Bits band (1 bsl Pin) =/= 0.
 
%%--------------------------------------------------------------------
gpio_set(Pin) when ?is_uint8(Pin) ->
    Bits = read_output(),
    write_output(Bits bor (1 bsl Pin)).

%%--------------------------------------------------------------------
gpio_clr(Pin) when ?is_uint8(Pin) ->
    Bits = read_output(),
    write_output(Bits band (bnot (1 bsl Pin))).

%%--------------------------------------------------------------------
read_input(SPI) ->
    spi_read(SPI,?INPUT_PORT).

%%--------------------------------------------------------------------
read_output(SPI) ->
    spi_read(SPI,?OUTPUT_PORT).

%%--------------------------------------------------------------------
write_output(SPI,Value) ->
    spi_write(SPI,?OUTPUT_PORT, Value).
		  
init([]) ->
	{ok,SPI}=spi:start_link(?SPIDEV, []),
	?info({spi_started,{pid,SPI}}),

	X=?IOCON_HAEN bor ?IOCON_MIRROR,

	A=spi:transfer(SPI,<< ?SPI_WRITE_CMD, ?IOCON,  X >>),
	B=spi:transfer(SPI,<< ?SPI_WRITE_CMD, ?IODIRA, 0 >>),     %% set port A as outputs
 	C=spi:transfer(SPI,<< ?SPI_WRITE_CMD, ?IODIRB, 16#FF >>), %% set port B as inputs
 	D=spi:transfer(SPI,<< ?SPI_WRITE_CMD, ?GPIOA,  16#FF >>), %% set port A on
 	E=spi:transfer(SPI,<< ?SPI_WRITE_CMD, ?GPPUA,  16#FF >>), %% set port A pullups on
    F=spi:transfer(SPI,<< ?SPI_WRITE_CMD, ?GPPUB,  16#FF >>), %% set port B pullups on
    G=write_output(SPI,16#00),       %% lower all outputs

 	?info({spi,{A,B,C,D,E,F,G}}),
 %    D=spi_write(?GPIOA,  16#FF), %% set port A on

	{ok,#ctx{spi=SPI}}.

	% ?info({init,?SPI_BUS,?SPI_DEVICE}),
 %    ok = spi:open(?SPI_BUS, ?SPI_DEVICE),
 %    ok = spi:debug(debug),	% TODO - remove this for prod
 %    ?info({spi_open,ok}),

 %    A=spi_write(?IOCON,  ?IOCON_HAEN bor ?IOCON_MIRROR),
 %    B=spi_write(?IODIRA, 0),     %% set port A as outputs
 %    C=spi_write(?IODIRB, 16#FF), %% set port B as inputs
 %    D=spi_write(?GPIOA,  16#FF), %% set port A on
 %    %% spi_write(?GPIOB,  0xFF), %% set port B on
 %    E=spi_write(?GPPUA,  16#FF), %% set port A pullups on
 %    F=spi_write(?GPPUB,  16#FF), %% set port B pullups on
 %    G=write_output(16#00),       %% lower all outputs

 %    StatusList=[A,B,C,D,E,F,G],
	
	% % TODO -- ensure all previous commands return ok otherwise exit 

	% case lists:all(fun(Z)->is(ok,Z) end,StatusList) of
	% 	true->
	% 		?info("piface initialised ok"),
	% 		{ok, #ctx{}};
	% 	_->
	% 		?critical({piface_init_failed,StatusList}),
	% 		{stop,{piface_init_failed,StatusList}}
	% end.

is(A,A)->
	true;

is(_,_)->
	false.

handle_call(stop, _From, Ctx) ->
    {stop, normal, ok, Ctx};

handle_call(_Request, _From, Ctx) ->
    {reply, {error, bad_call}, Ctx}.

handle_cast(_Msg, Ctx) ->
    {noreply, Ctx}.

handle_info(_Info, Ctx) ->
    {noreply, Ctx}.

code_change(_OldVsn, Ctx, _Extra) ->
    {ok, Ctx}.

terminate(_Reason, _Ctx) ->
    ok.

%%--------------------------------------------------------------------
%% Utilities
%%--------------------------------------------------------------------

spi_write(SPI,Port,Value)->
	spi:transfer(SPI, << ?SPI_WRITE_CMD, Port, Value >>);

spi_read(SPI,Port)->
	spi:transfer(SPI, << ?SPI_READ_CMD, Port, 16#ff >>);

% spi_write(Port, Value) ->
%     case spi:transfer(?SPI_BUS, ?SPI_DEVICE,
% 		      <<?SPI_WRITE_CMD, Port, Value>>,
% 		      ?TRANSFER_LEN,
% 		      ?TRANSFER_DELAY,
% 		      ?TRANSFER_SPEED,
% 		      ?TRANSFER_BPW, 0) of
% 	{ok,_Data} -> ok;
% 	Error -> Error
%     end.

% spi_read(Port) ->
%     case spi:transfer(?SPI_BUS, ?SPI_DEVICE,
% 		      <<?SPI_READ_CMD, Port, 16#ff>>,
% 		      ?TRANSFER_LEN,
% 		      ?TRANSFER_DELAY,
% 		      ?TRANSFER_SPEED,
% 		      ?TRANSFER_BPW, 0) of
% 	{ok, <<_,_,Bits>>} -> Bits;
% 	{ok, _} -> {error,badbits};
% 	Error -> Error
%     end.
