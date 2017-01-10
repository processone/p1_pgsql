%%% File    : pgsql_socket.erl
%%% Author  : Blah <cos@local>
%%% Description : Unwrapping of TCP/SSL line protocol packages to postgres messages.
%%% Created : 22 Jul 2005

-module(pgsql_socket).

-behaviour(gen_server).

-export([start/3, start_link/3]).

%% gen_server callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 code_change/3,
	 handle_info/2,
	 terminate/2]).

-record(state, {socket, sockmod, protopid, buffer, as_binary}).

start(Sock, ProtoPid, AsBin) ->
    gen_server:start(?MODULE, [Sock, ProtoPid, AsBin], []).

start_link(Sock, ProtoPid, AsBin) ->
    gen_server:start_link(?MODULE, [Sock, ProtoPid, AsBin], []).

init([{Mod, Sock}, ProtoPid, AsBin]) ->
    setopts({Mod, Sock}, [{active, once}]),
    {ok, #state{socket = Sock, sockmod = Mod, protopid = ProtoPid,
                buffer = <<>>, as_binary = AsBin}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info({Tag, Sock, Bin},
	    #state{socket = Sock,
		   sockmod = Mod,
		   protopid = ProtoPid,
                   as_binary = AsBin,
		   buffer = Buffer} = State)
  when Tag == tcp; Tag == ssl ->
    {ok, Rest} = process_buffer(ProtoPid, AsBin, <<Buffer/binary, Bin/binary>>),
    setopts({Mod, Sock}, [{active, once}]),
    {noreply, State#state{buffer = Rest}};
handle_info({Tag, Sock},
	    #state{socket = Sock, sockmod = Mod,
		   protopid = ProtoPid} = State)
  when Tag == tcp_closed; Tag == ssl_closed ->
    io:format("Sock closed~n", []),
    ProtoPid ! {socket, {Mod, Sock}, closed},
    {stop, tcp_close, State};
handle_info({Tag, Sock, Reason},
	    #state{socket = Sock, sockmod = Mod,
		   protopid = ProtoPid} = State)
  when Tag == tcp_error; Tag == ssl_error ->
    io:format("Sock error~n", []),
    ProtoPid ! {socket, {Mod, Sock}, {error, Reason}},
    {stop, tcp_error, State};
handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


%% Given a binary that begins with a proper message header the binary
%% will be processed for each full message it contains, and it will
%% return any trailing incomplete messages.
process_buffer(ProtoPid, AsBin,
               Bin = <<Code:8/integer, Size:4/integer-unit:8, Rest/binary>>) ->
    Payload = Size - 4,
    if
	size(Rest) >= Payload ->
	    <<Packet:Payload/binary, Rest1/binary>> = Rest,
	    {ok, Message} = pgsql_proto:decode_packet(Code, Packet, AsBin),
	    ProtoPid ! {pgsql, Message},
	    process_buffer(ProtoPid, AsBin, Rest1);
	true ->
	    {ok, Bin}
    end;
process_buffer(_ProtoPid, _AsBin, Bin) when is_binary(Bin) ->
    {ok, Bin}.

setopts({gen_tcp, Sock}, Opts) ->
    inet:setopts(Sock, Opts);
setopts({ssl, Sock}, Opts) ->
    ssl:setopts(Sock, Opts).
