%%%-------------------------------------------------------------------
%%% @author ngq <ngq_scut@126.com>
%%% @doc
%%% 使用sha256加密解密
%%% @end
%%%-------------------------------------------------------------------
-module(lib_token).

-export([
    generate_secret/3,
    generate_secret/4,
    verify_secret/3,
    verify_secret/4
]).

-export([
    encode_base16/1,
    encode_url_base64/2,
    decode_url_base64/2,
    uuid4/0
]).

%% @doc 生成秘钥
%% 指定生成时间 signed_at_seconds(时间戳：秒)
%% @end
-spec (generate_secret(term(), binary(), term()) -> binary()).
generate_secret(Data, SecretKey, Salt) ->
    generate_secret(Data, SecretKey, Salt, []).

-spec (generate_secret(term(), binary(), term(), list()) -> binary()).
generate_secret(Data, SecretKey, Salt, Opt) ->
    SignedAtMs = case lists:keyfind(signed_at_seconds, 1, Opt) of
        {_, Seconds} -> erlang:trunc(Seconds * 1000);
        _ -> erlang:system_time(1000)
    end,
    Secret = get_secret(SecretKey, Salt),
    Bin = term_to_binary(#{data => Data, signed => SignedAtMs}),
    message_sign(Bin, Secret).

message_sign(Bin, Secret) ->
    PlainText = list_to_binary([
        encode_url_base64(<<"HS256">>, [nopadding]),
        <<".">>,
        encode_url_base64(Bin, [nopadding])
    ]),
    Signature  = crypto:hmac(sha256, Secret, PlainText),
    list_to_binary([
        PlainText,
        <<".">>,
        encode_url_base64(Signature, [nopadding])
    ]).

%% @doc 解密秘钥
%% 秘钥有效时间 max_age(秒)
%% @end
-spec (verify_secret(binary(), binary(), term()) -> {ok, term()} | {error, invalid | expired}).
verify_secret(Token, SecretKey, Salt) ->
    verify_secret(Token, SecretKey, Salt, []).

-spec (verify_secret(binary(), binary(), term(), list()) -> {ok, term()} | {error, invalid | expired}).
verify_secret(Token, SecretKey, Salt, Opt) ->
    Secret = get_secret(SecretKey, Salt),
    [Protected, Payload, Signature] = binary:split(Token, <<".">>, [global]),
    NewProtected = decode_url_base64(Protected, [nopadding]),
    case NewProtected == <<"HS256">> of
        true ->
            PlainText = list_to_binary([Protected, <<".">>, Payload]),
            NewPayload = decode_url_base64(Payload, [nopadding]),
            NewSignature = decode_url_base64(Signature, [nopadding]),
            Challenge = crypto:hmac(sha256, Secret, PlainText),
            case secure_compare(Challenge, NewSignature, 0) of
                true ->
                    #{
                        data := Data,
                        signed := Signed
                    } = erlang:binary_to_term(NewPayload),
                    case lists:keyfind(max_age, 1, Opt) of
                    {_, MaxAgeS} ->
                        case trunc(MaxAgeS * 1000 + Signed) < erlang:system_time(1000) of
                            true ->
                                {error, expired};
                            false ->
                                {ok, Data}
                        end;
                    _ ->
                      {ok, Data}
                    end;
                false ->
                    {error, invalid}
            end;
        false ->
            {error, invalid}
    end.

secure_compare(<<X:8, Left/binary>>, <<Y:8, Right/binary>>, Acc) ->
    secure_compare(Left, Right, Acc bor (X bxor Y));
secure_compare(<<>>, <<>>, Acc) -> Acc == 0;
secure_compare(_, _, _) -> false.

%% 获取秘钥
-spec (get_secret(binary(), term()) -> binary()).
get_secret(SecretKey, Salt) when is_binary(Salt) ->
    do_get_secret(SecretKey, Salt, 1000, 32, 1, [], 0);
get_secret(SecretKey, Salt) ->
    get_secret(SecretKey, term_to_binary(Salt)).

do_get_secret(SecretKey, Salt, Iterations, MaxLen, BlockIndex, Acc, Len) when Len < MaxLen ->
    Initial = crypto:hmac(sha256, SecretKey, <<Salt/binary, BlockIndex:32>>),
    Block = iterate(Iterations - 1, SecretKey, Initial, Initial),
    do_get_secret(SecretKey, Salt, Iterations - 1, MaxLen, BlockIndex + 1, [Block|Acc], byte_size(Block) + Len);
do_get_secret(_SecretKey, _Salt, _Iterations, MaxLen, _BlockIndex, Acc, _Len) ->
    MaxBitLen = trunc(MaxLen * 8),
    <<Bin:MaxBitLen, _/binary>> = list_to_binary(lists:reverse(Acc)),
    <<Bin:MaxBitLen>>.

iterate(N, SecretKey, Prev, Acc) when N > 0 ->
    Next = crypto:hmac(sha256, SecretKey, Prev),
    iterate(N - 1, SecretKey, Next, crypto:exor(Next, Acc));
iterate(_N, _SecretKey, _Prev, Acc) -> Acc.

%% @doc 来源 https://github.com/goj/base16.git
-spec (encode_base16(binary()) -> binary()).
encode_base16(Bin) when is_binary(Bin) ->
    << <<(hex(N div 16)), (hex(N rem 16))>> || <<N>> <= Bin >>.

hex(N) when N < 10 ->
    N + $0;
hex(N) when N < 16 ->
    N - 10 + $a.

%% @doc 编码url_base64
-spec (encode_url_base64(binary(), list()) -> binary()).
encode_url_base64(Bin, Opt) ->
    B1 = base64:encode(Bin),
    B2 = binary:replace(B1, <<"+">>, <<"-">>, [global]),
    B3 = binary:replace(B2, <<"/">>, <<"_">>, [global]),
    case lists:member(nopadding, Opt) of
        true ->
            binary:replace(B3, <<"=">>, <<"">>, [global]);
        false ->
            B3
    end.

%% @doc 解码url_base64
-spec (decode_url_base64(binary(), list()) -> binary()).
decode_url_base64(Bin, Opt) ->
    B1 = binary:replace(Bin, <<"-">>, <<"+">>, [global]),
    B2 = binary:replace(B1, <<"_">>, <<"/">>, [global]),
    B3 = case lists:member(nopadding, Opt) of
        true ->
            N = (4 - (byte_size(B2) rem 4)) rem 4,
            Padding = list_to_binary(lists:duplicate(N, <<"=">>)),
            <<B2/binary, Padding/binary>>;
        false ->
            B2
    end,
    base64:decode(B3).

%% @doc 基于随机数的uuid
uuid4() ->
    <<U0:48, _:4, U1:12, _:2, U2:62>> = crypto:strong_rand_bytes(16),
    uuid_to_string(<<U0:48, 4:4, U1:12, 2:2, U2:62>>, default).

%% Convert UUID bytes to String.
uuid_to_string(<<U0:32, U1:16, U2:16, U3:16, U4:48>>, default) ->
    list_to_binary([
        binary_to_hex_list(<<U0:32>>), $-, binary_to_hex_list(<<U1:16>>), $-,
        binary_to_hex_list(<<U2:16>>), $-, binary_to_hex_list(<<U3:16>>), $-,
        binary_to_hex_list(<<U4:48>>)
    ]);
uuid_to_string(<<U:128>>, hex) ->
    list_to_binary(binary_to_hex_list(<<U:128>>));
uuid_to_string(<<U:128>>, urn) ->
    Bin = uuid_to_string(<<U:128>>, default),
    <<"urn:uuid:", Bin/binary>>.

binary_to_hex_list(Bin) ->
    list_to_hex_str(binary:bin_to_list(Bin), []).

list_to_hex_str([H|T], Res) when H < 256 ->
    list_to_hex_str(T, [to_hex(H rem 16), to_hex(H div 16)|Res]);
list_to_hex_str([], Res) ->
    lists:reverse(Res).

to_hex(C) when C < 10 -> 0 + C + 48;
to_hex(C) when C >= 10 andalso C < 16 -> $a + (C - 10).
