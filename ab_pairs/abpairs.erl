-module(abpairs).
%% Make a string of a specified length where the combinations of A's and B's in order
%% is a specified number.
%% Each string must have the specified number of characters and must have at least one A and one B.
%% A and B can be out of order, e.g. ABA (1 pair of AB: AB_)
%% e.g. 3,2 is a string of length 3 with 2 combinations of AB -> AAB, ABB
%% AAB is A_B and _AB
%% ABB is AB_ and A_B

%% This is really a combinatorial problem and I think I can get there by counting:
%% BA = 0     <- Can extend this out to any number of characters
%% AB = 1     <- Can extend this out to any number of characters
%% AAB = 2   ABB = 2
%% AAAB = 3  ABBB = 3
%% AABB = 4  AAAAB = 4  ABBBB = 4


%% So we can keep moving the A left so long as the number of pairs "P"
%% is less than the number of characters

%% ABBBB = 4
%% ABBAB = 4
%% ABABB = 5
%% AABBB = 6
%% AABBA = 4

%% What's the max P for length L

%% AAABBB = 9 = A * B

%% So count up from 0 to 9 in a string of length 6
%% 0 = BBBBBA
%% 1 = BBBBAB
%% 2 = BBBABB
%% 3 = BBABBB
%% 4 = BABBBB
%% 5 = ABBBBB = 1 * 5
%% 6 = AABBBA = 2 * 3
%% 7 = AABBAB = 2 * 3 + 1 * 1
%% 8 = AAAABB = 4 * 2        (or AABBBB = 2 * 4)
%% 9 = AAABBB = 3 * 3

%% So if the factors add up to L then you can get P
%% Can we do it if the factors of L-1 add up to P - 1 ... this leaves us another A that we can slot in:
%% AABBB is only 5 characters long, so we can add an A somewhere in the Bs
%% 4 = BBAABB or AABBAA
%% 5 = AABBAB
%% 6 = AABBBA
%% 7 = AABBAB
%% 8 = AABABB
%% 9 = AA>A<BBB

%% so we can pad the front with B's, the back with A's, or both the front with B's and the back with A's




%% Josh's swap forward
%% ABBB
%% BABB <- the first two letters are AB, which are swapped 

%% AABB
%% ABB A
%% ABAB <- 2nd and 3rd are swapped, first A is added back on the front and last B is added back on the end

%% ABAB
%% BAAB <- first pair is swapped

%% BAAB
%% AAB B
%% AB AB
%% BABA <- the [AB] that came of the front (as BA) is reversed back (to BA) and added back and the last AB are swapped

%% Swap forward always gives us one less AB pair


%% Could we also count up?

%% 0 = BBBBBA
%% 1 = BBBBAB
%% 2 = BBBABB
%% 3 = BBABBB
%% 4 = BABBBB
%% 5 = ABBBBB
%% 5 = ABBBAB <- That's the same as above, we took away a B which robbed us of a pair, so we need to replace it and add another one
%% 6 = ABBABB
%% 7 = ABABBB
%% 8 = AABBBB
%% 8 = AABABB <- That's the same as above, when you take away a B after X A's you lose X pairs
%% 9 = AAABBB

%% either a sum (A + B) where A * B multiplies to K or a sum of N - 1 (A + B) where A * B + (N - 1 - A) where the addends multiple to K - A * B

%% So for 6,8 the addends for N could be 1,5 2,4, 3,3, which multiple to 5, 8, 9, our solution is 2,4
%% ... 6,7 -> 1,5 2,4, 3,3 -> nothing matches, so how about addends of 6 - 1: 1,4 2,3 which multiply to 4 and 6, with 2,3 we have addends 1,2 (or 2,1)
%% AABBB
%% AABBAB <- 7  ... we're adding 1 A in amongst 3 Bs, it could have been 1 x 3, 1 x 2, 1 x 1 so it can be multiples of 1,3 1,2 1,1
%% So really it's the multiple of the (N - 1) addends (C, D) plus any of the mulitples of 1 and X where X <= B

%% What about addends of N - 2 multiplied?
%% AABB -> 1,3 2,2 3,1 (3, 4, 3) -> We're adding in two A's so their going to add to four and multiple to either 3, 4 or 3 again ... OR they can add to 3 and multiple to 2
%% Oh, this is the solution for N-1 + the solution for (N - X) minus A where we solve for X A's

%% So what's the solution to N -> it's the addends of N multiplied

%% Find the addends

addends(X) ->
    [{X - I, I} || I <- lists:seq(0, X div 2)].


%% now multiply the addends and see if any match K (# of pairs)
multiples(Pairs) ->
    [X * Y || {X, Y} <- Pairs]. %% or map the * function over it, I don't think their is one

%% Take the highest one first. If none work, then either fill in the first slot with B or the last slot with A and solve for the rest

%% filter for answer
has_answer(Multiples, K) ->
    lists:member(K, Multiples).

addends(X, A) ->
    [{I, A} || I <- lists:seq(0, I - A)].

%% use multiples again
    %%

%% Oh, the 2nd step is just like the first where we can put a B in front if A___ is two big


%% The second problem is: given a string of B's, can I make X pairs with 1 A?
%% The number of pairs is just the number of B's, so if #Bs >= A then yes

%% Can we get K with addends?
%%  yes? -> we're done
%%  no?
%%      Can get K with N-1 addends + #Bs?
%%      yes? -> K, figure out the higest addend pair and add the appropriate A before the B's
%%      no?
%%          Can we get K with N-2 addends + 1 A + 1 A    <- what's a scenario that would fit?
%%                                                          AABAAB (6,6) -> BAABBB ... could solve that with padding
%%          If we're solving for multiple A's and a set number of Bs, is there a way to calculate that easily?
%%                  Should be the same as the original: multiplied addends where the B addend is known
%%                  If we're to high then we pad
%%


%% generate addend multiples
addend_multiples(X) ->
    [{{X - Y, Y}, (X - Y) * Y} || Y <- lists:seq(1, X div 2)].

%% find matching K for list of N addend multiples
check(Y, Xs) ->
    Filter = fun(X = {_, Z}) when Z == Y ->
                 true;
                (_) ->
                  false
             end,
    case lists:filter(Filter, Xs) of
        [M] ->
            M;
        _ ->
            undefined
    end.

%% Get the best match of the addend multiples
best(Target, Pairs) ->
    Sort = fun({_, X}, {_, Y}) ->
                   X =< Y
           end,
    Lower = fun({_, X}) when X =< Target ->
                     true;
                (_) ->
                     false
             end,
    case lists:filter(Lower, Pairs) of
        [] ->
            [];
        LowerPairs ->
            hd(lists:reverse(lists:sort(Sort, LowerPairs)))
    end.


