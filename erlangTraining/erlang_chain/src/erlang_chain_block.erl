-module(erlang_chain_block).

-export([ new/3 ]).
-export_type([ block/0 ]).

-export([ get_data/1
        , get_index/1
        , get_timestamp/1
        , get_nonce/1
        , get_hash/1
        ]).

-type data() :: map().
-type index() :: non_neg_integer().
-type timestamp() :: non_neg_integer().
-type nonce() :: non_neg_integer().
-type difficulty() :: non_neg_integer().
-type hash() :: binary().

-record(block, { data :: data()
               , index :: index()
               , timestamp :: timestamp()
               , nonce :: nonce()
               , hash :: hash()
               }).
        
-opaque block() :: #block{}.

-spec get_data(Block :: block()) -> data().
get_data(#block{ data = Data }) -> Data.

-spec get_index(Block :: block()) -> index().
get_index(#block{ index = Index }) -> Index.

-spec get_timestamp(Block :: block()) -> timestamp().
get_timestamp(#block{ timestamp = Timestamp }) -> Timestamp.

-spec get_nonce(Block :: block()) -> nonce().
get_nonce(#block{ nonce = Nonce }) -> Nonce.

-spec get_hash(Block :: block()) -> hash().
get_hash(#block{ hash = Hash }) -> Hash.

-spec new(Data :: data(), Index :: index(), Difficulty :: difficulty()) -> erlang_chain_block:block().
new(Data, Index, Difficulty) -> 
    Time = erlang:system_time(seconds),
    Hash = compute_hash(Data, Index, Time, 0),
    {Nonce, Hash1} = case Difficulty of 
                        0 -> {0, Hash};
                        _ -> mine_block(Data, Index, Time, Difficulty, 0, Hash)
                    end,

    #block{ data = Data
          , index = Index
          , timestamp = Time
          , nonce = Nonce
          , hash = Hash1
          }.

-spec compute_hash(Data :: data(), Index :: index(), Timestamp :: timestamp(),
            Nonce :: nonce()) -> binary().

compute_hash(Data, Index, Timestamp, Nonce) ->
    Body = jsone:encode(Data),
    Index1 = integer_to_binary(Index),
    Timestamp1 = integer_to_binary(Timestamp),
    Nonce1 = integer_to_binary(Nonce),
    Full = <<Body/binary, Index1/binary, Timestamp1/binary, Nonce1/binary>>,
    Hash = crypto:hash(sha256, Full),
    base16:encode(Hash).

-spec mine_block(Data :: data(), Index :: index(), Timestamp :: timestamp(),
                 Difficulty :: difficulty(), Nonce :: nonce(), Hash :: hash()) ->
  {nonce(), hash()}.
mine_block(Data, Index, Timestamp, Difficulty, Nonce, Hash) ->
    io:format("mining a new block: ~p~n", [{Nonce, Difficulty}]),
    case binary:match(Hash, binary:copy(<<"0">>, Difficulty)) of
        {0, _} -> {Nonce, Hash};
        _ ->
        Nonce1 = Nonce + 1,
        Hash1 = compute_hash(Data, Index, Timestamp, Nonce1),
        mine_block(Data, Index, Timestamp, Difficulty, Nonce1, Hash1)
    end.