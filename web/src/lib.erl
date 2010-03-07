-module(lib).
-export([ is_defined/2 ]).
-include_lib("webmachine/include/webmachine.hrl").

is_defined([], _) ->
    true;

is_defined([Value | T ], Plist) ->
    case proplists:is_defined(Value, Plist) of
        false ->
            false;
        true ->
            is_defined(T, Plist)
    end.
