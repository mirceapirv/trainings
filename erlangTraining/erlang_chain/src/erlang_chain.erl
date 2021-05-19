-module(erlang_chain).

-export([ new/1
        , add_new_block/2 ]).

-type difficulty() :: non_neg_integer().
-type data() :: #{term() => binary()}.
-type chain() :: {difficulty(), [erlang_chain_block:block()]}.

-spec new(Difficulty :: difficulty()) -> chain().
new(Difficulty) -> 
    Genesis = create_genesis_block(),
    {Difficulty, [Genesis]}.

-spec add_new_block(Chain :: chain(), BlockData :: data()) -> chain().
add_new_block({Difficulty, Chain}, BlockData) ->
    Block = erlang_chain_block:new(BlockData, length(Chain), Difficulty),
    {Difficulty, [ Block | Chain ]}.

create_genesis_block() ->
    erlang_chain_block:new(#{ genesis => <<"true">>}, 0, 0).