-module(piface2).
-behaviour(gen_server).

-include_lib("picontroller/include/debug.hrl").

-define(is_uint8(T), (((T) band (bnot 16#ff)) =:=  0)).
-define(is_uint16(T), (((T) band (bnot 16#ffff)) =:=  0)).
-define(is_uint32(T), (((T) band (bnot 16#ffffffff)) =:=  0)).


-export([
	gpio_get/1, 
	gpio_set/1, 
	gpio_clr/1
]).

-export([
	read_input/0, 
	read_output/0,
	write_output/1]).

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

% -define(info(Str),		lager:info("~p",[Str])).
% -define(critical(Str),	lager:critical("~p",[Str])).

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

read_input()->
	gen_server:call(?PIFACE_SRV, read_input).

read_output()->
	gen_server:call(?PIFACE_SRV, read_output).

write_output(Value)->
	gen_server:call(?PIFACE_SRV, {write_output,Value}).

gpio_get(Pin)->
	gen_server:call(?PIFACE_SRV,{gpio_get,Pin}).

gpio_set(Pin)->
	gen_server:call(?PIFACE_SRV,{gpio_set,Pin}).

gpio_clr(Pin)->
	gen_server:call(?PIFACE_SRV,{gpio_clr,Pin}).

%%--------------------------------------------------------------------
init_interrupt(SPI) ->
    spi_write(SPI, ?INTCONB,  16#00), %% interrupt on any change
    spi_write(SPI, ?GPINTENB, 16#FF), %% enable interrupts on B
    ok.

%%--------------------------------------------------------------------
i_read_input(SPI) ->
    << _A,_B,C >> = spi_read(SPI,?INPUT_PORT),
    C.

%%--------------------------------------------------------------------
i_read_output(SPI) ->
    << _A,_B,C >> = spi_read(SPI,?OUTPUT_PORT),
    C.

%%--------------------------------------------------------------------
i_write_output(SPI,Value) ->
    << _A,_B,C >> = spi_write(SPI,?OUTPUT_PORT, Value),
    C.
		  
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
    G=i_write_output(SPI,16#00),       %% lower all outputs
    init_interrupt(SPI),

 	?info({spi,{A,B,C,D,E,F,G}}),

	{ok,#ctx{spi=SPI}}.


handle_call(read_input, _From, Ctx=#ctx{spi=SPI}) ->
	Input=i_read_input(SPI),
    {reply, Input, Ctx};

handle_call(read_output, _From, Ctx=#ctx{spi=SPI}) ->
	Input=i_read_output(SPI),
    {reply, Input, Ctx};

handle_call({write_output,Value}, _From, Ctx=#ctx{spi=SPI}) ->
	Reply=i_write_output(SPI,Value),
    {reply, Reply, Ctx};

handle_call({gpio_get,Pin}, _From, Ctx=#ctx{spi=SPI}) when ?is_uint8(Pin) ->
    Bits = i_read_input(SPI),
    Reply = (Bits band (1 bsl Pin) =/= 0),
    {reply, Reply, Ctx};

handle_call({gpio_set,Pin}, _From, Ctx=#ctx{spi=SPI}) when ?is_uint8(Pin) ->
    Bits = i_read_output(SPI),
    Reply=i_write_output(SPI,Bits bor (1 bsl Pin)),
    {reply, Reply, Ctx};

handle_call({gpio_clr,Pin}, _From, Ctx=#ctx{spi=SPI}) when ?is_uint8(Pin)->
    Bits = i_read_output(SPI),
    Reply=i_write_output(SPI,Bits band (bnot (1 bsl Pin))),
    {reply, Reply, Ctx};

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
	spi:transfer(SPI, << ?SPI_WRITE_CMD, Port, Value >>).

spi_read(SPI,Port)->
	spi:transfer(SPI, << ?SPI_READ_CMD, Port, 16#ff >>).