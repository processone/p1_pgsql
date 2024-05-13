%%%-------------------------------------------------------------------
%%% @author Alexey Shchepin <alexey@process-one.net>
%%%
%%% Copyright (C) 2002-2024 ProcessOne, SARL. All Rights Reserved.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%%-------------------------------------------------------------------
-module(pgsql_sasl).
-author('alexey@process-one.net').

-export([client_new/3, client_step/2, client_finish/2]).

-record(sasl_state,
        {user          :: binary(),
         password      :: binary(),
         nonce         :: binary(),
         verify = <<>> :: binary()
}).

-spec client_new(binary(), binary(), list(binary())) ->
                        {ok, binary(), binary(), #sasl_state{}} |
                        {error, any()}.
client_new(User, Password, Mechs) ->
    case lists:member(<<"SCRAM-SHA-256">>, Mechs) of
        true ->
            Nonce = base64:encode(crypto:strong_rand_bytes(18)),
            State = #sasl_state{user = User,
                                password = Password,
                                nonce = Nonce},
            Msg = client_first_message_bare(User, Nonce),
            Response = <<"n,,", Msg/binary>>,
            {ok, <<"SCRAM-SHA-256">>, Response, State};
        false ->
            {error, "No supported SASL mechs"}
    end.

-spec client_step(#sasl_state{}, binary()) ->
                         {ok, binary(), #sasl_state{}} |
                         {error, any()}.
client_step(State, ServerResponse) ->
    case parse(ServerResponse) of
        SResp when is_list(SResp) ->
            I = binary_to_integer(proplists:get_value(<<"i">>, SResp)),
            R = proplists:get_value(<<"r">>, SResp),
            S = base64:decode(proplists:get_value(<<"s">>, SResp)),
            Nonce = State#sasl_state.nonce,
            NonceSize = size(Nonce),
            case R of
                <<Nonce:NonceSize/binary, _/binary>> ->
                    ClientMsg1 = client_first_message_bare(
                                   State#sasl_state.user,
                                   State#sasl_state.nonce),
                    ClientMsg2 = <<"c=biws,r=", R/binary>>,
                    AuthMessage =
                        <<ClientMsg1/binary, ",",
                          ServerResponse/binary, ",",
                          ClientMsg2/binary>>,
                    Password = State#sasl_state.password,
                    SaltedPassword = scram:salted_password(
                                       sha256, Password, S, I),
                    ClientKey =
                        scram:client_key(sha256, SaltedPassword),
                    StoredKey = scram:stored_key(sha256, ClientKey),
                    ClientSignature =
                        scram:client_signature(sha256, StoredKey, AuthMessage),
                    ClientProof =
                        crypto:exor(ClientKey, ClientSignature),
                    P = base64:encode(ClientProof),
                    Msg = <<ClientMsg2/binary, ",p=", P/binary>>,
                    ServerKey =
                        scram:server_key(sha256, SaltedPassword),
                    V = scram:server_signature(sha256, ServerKey, AuthMessage),
                    {ok, Msg, State#sasl_state{nonce = R, verify = V}};
                _ ->
                    {error, "Bad SASL server nonce"}
            end;
        _ ->
            {error, {"Error parsing server response", ServerResponse}}
    end.

-spec client_finish(#sasl_state{}, binary()) -> ok | {error, any()}.
client_finish(State, ServerResponse) ->
    case parse(ServerResponse) of
        SResp when is_list(SResp) ->
            V = base64:decode(proplists:get_value(<<"v">>, SResp)),
            if
                State#sasl_state.verify == V ->
                    ok;
                true ->
                    {error, "SASL server verification failed"}
            end;
        _ ->
            {error, {"Error parsing server response", ServerResponse}}
    end.

client_first_message_bare(User, Nonce) ->
    <<"n=", User/binary, ",r=", Nonce/binary>>.

parse(S) -> parse1(S, <<>>, []).

parse1(<<$=, Cs/binary>>, S, Ts) ->
    parse2(Cs, S, <<>>, Ts);
parse1(<<$,, Cs/binary>>, <<>>, Ts) -> parse1(Cs, <<>>, Ts);
parse1(<<$\s, Cs/binary>>, <<>>, Ts) -> parse1(Cs, <<>>, Ts);
parse1(<<C, Cs/binary>>, S, Ts) -> parse1(Cs, <<S/binary, C>>, Ts);
parse1(<<>>, <<>>, T) -> lists:reverse(T);
parse1(<<>>, _S, _T) -> bad.

parse2(<<$", Cs/binary>>, Key, Val, Ts) ->
    parse3(Cs, Key, Val, Ts);
parse2(<<C, Cs/binary>>, Key, Val, Ts) ->
    parse4(Cs, Key, <<Val/binary, C>>, Ts);
parse2(<<>>, _, _, _) -> bad.

parse3(<<$", Cs/binary>>, Key, Val, Ts) ->
    parse4(Cs, Key, Val, Ts);
parse3(<<$\\, C, Cs/binary>>, Key, Val, Ts) ->
    parse3(Cs, Key, <<Val/binary, C>>, Ts);
parse3(<<C, Cs/binary>>, Key, Val, Ts) ->
    parse3(Cs, Key, <<Val/binary, C>>, Ts);
parse3(<<>>, _, _, _) -> bad.

parse4(<<$,, Cs/binary>>, Key, Val, Ts) ->
    parse1(Cs, <<>>, [{Key, Val} | Ts]);
parse4(<<$\s, Cs/binary>>, Key, Val, Ts) ->
    parse4(Cs, Key, Val, Ts);
parse4(<<C, Cs/binary>>, Key, Val, Ts) ->
    parse4(Cs, Key, <<Val/binary, C>>, Ts);
parse4(<<>>, Key, Val, Ts) ->
    parse1(<<>>, <<>>, [{Key, Val} | Ts]).

