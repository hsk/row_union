:- op(600, xfy, [<:]).
:- op(650, yfx, [$]).

T <: U :- get_history(Hist), member(X<:Y, Hist), X == T, Y == U, !.
T <: U :- get_history(Hist), b_setval(sub_history, [T<:U|Hist]),sub_core(T, U).
get_history(Hist) :- catch(b_getval(sub_history, Hist), _, Hist = []).
sub_core(T, T) :- !.
sub_core(T1, T2) :- is_list(T1), is_list(T2), !, sub_list(T1, T2).
sub_core((A1 -> B1), (A2 -> B2)) :- !, A2 <: A1, B1 <: B2, !.
sub_list([], _):- !.
sub_list([X|Xs], Ys) :- member(Y, Ys), elem_sub(X, Y), sub_list(Xs, Ys), !.
elem_sub(C$Args1, C$Args2) :- !, maplist(<:, Args1, Args2).
elem_sub(X, X).

check(_, I, [int]) :- integer(I), !.
check(_, true, [bool]) :- !.
check(_, false, [bool]) :- !.
check(Γ, X, T) :- atom(X), !, member(X:T, Γ).
check(Γ, fix(X, E), T) :- !, check([X:T|Γ], E, T).
check(Γ, λ(X, E), T1->T2) :- !, check([X:T1|Γ], E, T2).
check(Γ, C$Es, [C$Ts]) :- is_list(Es),!,maplist(check(Γ), Es, Ts).
check(Γ, E1$E2, T2) :- !, check(Γ, E1, T1->T2), check(Γ, E2, T3), T3 <: T1.
check(Γ, E1+E2, [int]) :- !, check(Γ, E1, T1), T1 <: [int], check(Γ, E2, T2), T2 <: [int].
check(Γ, match(E, Cs), T) :- !, check(Γ, E, T2), cases(Γ, Cs, T, T3), T2 <: T3.

cases(_Γ, [], _, []).
cases(Γ, [default->E1|_], T2, T1) :- !, check([default:[T1]|Γ], E1, T2).
cases(Γ, [Pat->E1|Cs], T2, [Head|T3]) :- !,
    pat(Pat, [Head], Delta), append(Γ, Delta, Γ4),
    check(Γ4, E1, T_E1),
    cases(Γ, Cs, T_Cs, T3),
    union_types(T_E1, T_Cs, T2).

union_types(T, T, T) :- !.
union_types(T1, T2, T3) :- is_list(T1), is_list(T2), !, append(T1, T2, T3).

pat(I, [int], []) :- integer(I), !.
pat(true, [bool], []) :- !.
pat(false, [bool], []) :- !.
pat(X, T, [X:T]) :- atom(X), !.
pat(X:T, T, [X:T]) :- atom(X), !.
pat(C$Args, [C$Ts], Delta) :- !, maplist(pat,Args, Ts, Deltas),flatten(Deltas,Delta).

eval(I, _, I) :- integer(I), !.
eval(true, _, true) :- !.
eval(false, _, false) :- !.
eval(X, Env, Val) :- atom(X), member((X, Val), Env), !.
eval(λ(X, E), Env, closure(X, E, Env)) :- !.
eval(fix(X, E), Env, Val) :-
    eval(E, [(X, Val) | Env], Val), !.
eval(C $ Es, Env, C $ Vals) :- 
    atom(C), is_list(Es), !, 
    maplist(eval_env(Env), Es, Vals).
eval(E1 $ E2, Env, Result) :- 
    !, 
    eval(E1, Env, closure(X, Body, ClosureEnv)),
    eval(E2, Env, Val),
    eval(Body, [(X, Val) | ClosureEnv], Result).
eval(E1 + E2, Env, R) :- 
    !, 
    eval(E1, Env, R1), 
    eval(E2, Env, R2), 
    R is R1 + R2.
eval(match(E, Cs), Env, Result) :- 
    !, 
    eval(E, Env, Val), 
    eval_cases(Cs, Val, Env, Result).
eval_env(Env, E, V) :- eval(E, Env, V).

eval_cases([default -> Body | _], Val, Env, Result) :- 
    !, 
    eval(Body, [(default, Val) | Env], Result).
eval_cases([Pat -> Body | Rest], Val, Env, Result) :-
    (match_pattern(Pat, Val, Env, ExtendedEnv) ->
        eval(Body, ExtendedEnv, Result)
    ;
        eval_cases(Rest, Val, Env, Result)
    ).

match_pattern(I, I, Env, Env) :- integer(I), !.
match_pattern(true, true, Env, Env) :- !.
match_pattern(false, false, Env, Env) :- !.
match_pattern(X:T, Val, Env, [(X, Val) | Env]) :- 
    atom(X), !, 
    check_val_type(Val, T).
match_pattern(X, Val, Env, [(X, Val) | Env]) :- atom(X), !.
match_pattern(C $ Args, C $ Vals, Env, ExtendedEnv) :- 
    atom(C), is_list(Args), is_list(Vals), !,
    match_nested_patterns(Args, Vals, Env, ExtendedEnv).

match_nested_patterns([], [], Env, Env).
match_nested_patterns([P|Ps], [V|Vs], Env, ExtendedEnv) :-
    match_pattern(P, V, Env, Env1),
    match_nested_patterns(Ps, Vs, Env1, ExtendedEnv).

check_val_type(Val, [int]) :- integer(Val), !.
check_val_type(Val, [bool]) :- (Val == true ; Val == false), !.
check_val_type(C $ Vals, [C $ Ts]) :- 
    atom(C), is_list(Vals), is_list(Ts), !,
    catch(maplist(check_val_type, Vals, Ts), _, fail).

:- begin_tests(type_system).
    test(sub_union) :- [int, bool] <: [bool, int], !.
    test(sub_union2) :- [int, bool] <: [int, bool, string], !.
    test(sub_union_fail, [fail]) :- [int, bool, string] <: [int, bool].
    test(sub_arrow) :- ([int, bool] -> [int]) <: ([int] -> [int, bool]), !.
    test(lam_apply) :- check([], λ(x, x+1)$1, T), T == [int].
    test(typecase_match) :- 
        check([v:[int,bool,string]],match(v,[x:[int]->x,x:[bool]->1,default->0]),T),
        T == [int], !.
    test(ev_constructor_pattern) :-
        E = fix(ev, λ(x, match(x, [
                i:[int] -> i,
                add$[l, r] -> (ev $ l) + (ev $ r)
            ]))),
        check([], E, T),!,T=(T1->[int]),T1=[int,add$[T1,T1]].
    test(ev_constructor_pattern_apply) :-
        E = fix(ev, λ(x, match(x, [
                i:[int] -> i,
                add$[l, r] -> (ev $ l) + (ev $ r)
            ]))),
        check([], E $ (add$[add$[1, 2], 3]), T),!,T=[int].
    test(ev_constructor) :-
        check([], add$[add$[1, 2], 3], T), !,
        E = [int, add$[E,E]], T <: E.
    test(nested_constructor_pattern) :-
        E = λ(x, match(x, [
                add$[i:[int], r] -> i + 1,
                default -> 0
            ])),
        check([], E, T), !,
        T = ([add$[[int], _]] -> [int]).
    test(deep_nested_constructor_pattern) :-
        E = fix(ev,λ(x, match(x, [
                add$[add$[l1, r1], r] -> (ev$l1) + (ev$r1) + (ev$r),
                i:[int] -> i
            ]))),
        check([], E, T), !, T3 = [add$[T3,T3],int], (T3 -> [int]) <: T.
    test(match_return_different_types) :-
        check([v:[i$[int], b$[bool]]], match(v, [
            i$[x] -> b$[true],
            b$[x] -> i$[1]
        ]), T),
        T == [b$[[bool]], i$[[int]]], !.

    test(eval_lam_apply) :-
        eval(λ(x, x+1)$1, [], Result),
        Result == 2.
    test(eval_typecase_match_int) :-
        eval(match(v, [x:[int]->x, x:[bool]->1, default->0]), [(v, 5)], Result),
        Result == 5.
    test(eval_typecase_match_bool) :-
        eval(match(v, [x:[int]->x, x:[bool]->1, default->0]), [(v, true)], Result),
        Result == 1.
    test(eval_typecase_match_default) :-
        eval(match(v, [x:[int]->x, x:[bool]->1, default->0]), [(v, other_variant)], Result),
        Result == 0.
    test(eval_constructor_pattern_apply) :-
        E = fix(ev, λ(x, match(x, [
                i:[int] -> i,
                add$[l, r] -> (ev $ l) + (ev $ r)
            ]))),
        eval(E $ (add$[add$[1, 2], 3]), [], Result),
        Result == 6.
    test(eval_nested_constructor_pattern) :-
        E = λ(x, match(x, [
                add$[i:[int], r] -> i + 1,
                default -> 0
            ])),
        eval(E $ (add$[5, 10]), [], Result1), Result1 == 6,
        eval(E $ (add$[true, 10]), [], Result2), Result2 == 0.
:- end_tests(type_system).

:- set_prolog_flag(plunit_output, always).
:- run_tests.
:- halt.
