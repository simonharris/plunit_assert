:- module(plunit_assert, [
    assert_equals/2,
    assert_not_equals/2,
    assert_gt/2,
    assert_gte/2,
    assert_lt/2,
    assert_lte/2,
    assert_is/2,
    assert_is_not/2,
    assert_false/1,
    assert_true/1,
    assert_unbound/1,
    assert_not_unbound/1,
    assert_in/2,
    assert_not_in/2,
    assert_type/2,
    assert_not_type/2,
    assert_output/3,
    % Meta stuff - not really part of the plunit_assert API
    % assert_test_feedback/2,
    assert_test_fails/1,
    assert_test_passes/1
]).
/** <module> The test API for plunit_assert

A unit testing library for Prolog, providing an expressive xUnit-like API for
PlUnit, and better feedback to the user on test fail.

@author Simon Harding <github@pointbeing.net>
@license MIT
*/
:- dynamic prolog:assertion_failed/2.
:- multifile prolog:assertion_failed/2.

%! assert_true(:Goal) is semidet
%
% Test that Goal succeeds and therefore is truthy
%
% @arg Goal The goal to be tested
% @see assertion/1
assert_true(Cond) :-
    call_protected(Cond, fail_assert_true(Cond)).

fail_assert_true(Goal) :-
    feedback('Asserted true but got false for: ~q', [Goal]).

%! assert_false(:Goal) is semidet
%
% Test that Goal fails and therefore is falsy
%
% @arg Goal The goal to be tested
% @see assert_true/1
assert_false(Cond) :-
    call_protected((\+ Cond), fail_assert_false(Cond)).

fail_assert_false(Goal) :-
    feedback('Asserted false but got true for: ~q', [Goal]).

%! assert_equals(+A, +B) is semidet
%
% This is a superset of assert_is/2 and arithmetic comparison with =:=
%
% @arg A The first of the terms to be compared
% @arg B The second of the terms to be compared
assert_equals(A, B) :-
    call_protected(A == B; A =:= B, fail_assert_equals(A, B)).

fail_assert_equals(A, B) :-
    feedback('Asserted equal but ~q and ~q are not equal', [A, B]).

%! assert_not_equals(+A, +B) is semidet
%
% Test that A and B are not equal terms
%
% @arg A The first of the terms to be compared
% @arg B The second of the terms to be compared
% @see assert_equals/2
assert_not_equals(A, B) :-
    call_protected(\+ equal_after_eval(A, B),
                   fail_assert_not_equals(A, B)).

% this differs somewhat from assert_equals/2 at the moment, but this general
% approach may be more suitable once we start comparing compound terms etc
equal_after_eval(A, B) :-
    catch(ValA is A, _, fail),
    catch(ValB is B, _, fail),
    ValA =:= ValB, !.
equal_after_eval(A, B) :-
    A == B.

fail_assert_not_equals(A, B) :-
    feedback('Asserted ~q and ~q are not equal, but they are', [A, B]).

%! assert_is(+A, +B) is semidet
%
% Test that A and B are identical terms
%
% Uses ==/2 to check for term identity, which means it compares the terms A and
% B structurally, including the functor and arity (number of arguments) of the
% terms and the equality of each corresponding argument. Thus, succeeds if A
% and B are identical terms, without attempting to unify variables or perform
% any arithmetic evaluations
%
% @arg A The first of the terms to be compared
% @arg B The second of the terms to be compared
% @see assertion/1
% @see ==/2
assert_is(A, B) :-
    call_protected(A == B, fail_assert_is(A, B)).

fail_assert_is(A, B) :-
    feedback('Asserted identity but ~q and ~q are not identical', [A, B]).

%! assert_is_not(+A, +B) is semidet
%
% Test that A and B are not identical terms
%
% @arg A The first of the terms to be compared
% @arg B The second of the terms to be compared
% @see assert_is/2
assert_is_not(A, B) :-
    call_protected(A \== B, fail_assert_is_not(A, B)).

fail_assert_is_not(A, B) :-
    feedback('Asserted ~q and ~q are not identical, but they are', [A, B]).

%! assert_unbound(+Var) is semidet
%
% Test that Var is unbound
%
% This is analogous to isNull() or isNone() in other xUnit implementations
%
% @arg Var The variable to be tested for boundness
% @see assertion/1
assert_unbound(Var) :-
    call_protected(var(Var), fail_assert_unbound(Var)).

fail_assert_unbound(Var) :-
    feedback('Assertion that variable is unbound failed: it was bound to ~w', [Var]).

%! assert_not_unbound(+Var) is semidet
%
% Test that Var is not unbound
%
% @arg Var The variable to be tested for unboundness
% @see assert_unbound/1
assert_not_unbound(Var) :-
    call_protected(nonvar(Var), fail_assert_not_unbound(Var)).

fail_assert_not_unbound(_) :-
    feedback('Assertion that variable is bound failed: it was unbound', []).

%! assert_in(+Var, +Collection) is semidet
%
% Test that Var is in Collection
%
% This checks for list/set membership, and also whether Var is a valid
% dictionary key in Collection
%
% @arg Var The needle
% @arg Collection The haystack
% @see assertion/1
assert_in(Var, Collection) :-
    call_protected((
        member(Var, Collection) ;
        get_dict(Var, Collection, _)
    ), fail_assert_in(Var, Collection)).

fail_assert_in(Var, Collection) :-
    feedback('Asserted ~w is in ~w, but it isn\'t', [Var, Collection]).

%! assert_not_in(+Var, +Collection) is semidet
%
% Test that Var is not in Collection
%
% This checks for list/set membership, and also whether Var is a valid
% dictionary key in Collection
%
% @arg Var The needle
% @arg Collection The haystack
% @see assert_in/2
assert_not_in(Var, Collection) :-
    call_protected(\+ (
        member(Var, Collection) ;
        ( is_dict(Collection), get_dict(Var, Collection, _) )
    ), fail_assert_not_in(Var, Collection)).

fail_assert_not_in(Var, Collection) :-
    feedback('Asserted ~w is not in ~w, but it is', [Var, Collection]).

%! assert_type(+Term, +Type) is semidet
%
% Test that Var is of type Type
%
% Supported types are: number, integer, float, atom, compound, list, dict
%
% @arg Term The term to be tested
% @arg Type The type to be asserted
% @tbd Compound types
% @see assertion/1
% @tbd Consider must_be/2 or similar
assert_type(Term, float) :- call_protected(float(Term), fail_assert_type(float, Term)).
assert_type(Term, integer) :- call_protected(integer(Term), fail_assert_type(integer, Term)).
assert_type(Term, number) :- call_protected(number(Term), fail_assert_type(number, Term)).
assert_type(Term, atom) :- call_protected(atom(Term), fail_assert_type(atom, Term)).
assert_type(Term, compound) :- call_protected(compound(Term), fail_assert_type(compound, Term)).
assert_type(Term, list) :- call_protected(is_list(Term), fail_assert_type(list, Term)).
assert_type(Term, dict) :- call_protected(is_dict(Term), fail_assert_type(dict, Term)).

fail_assert_type(Expected, Term) :-
    term_type(Term, Got),
    feedback('Asserted ~w is of type \'~w\' but got \'~w\'', [Term, Expected, Got]).

%! assert_not_type(+Term, +Type) is semidet
%
% Test that Var is not of type Type
%
% @arg Term The term to be tested
% @arg Type The type to be un-asserted
% @see assert_type/2
assert_not_type(Term, float) :- call_protected(\+ float(Term), fail_assert_not_type(float, Term)).
assert_not_type(Term, integer) :- call_protected(\+ integer(Term), fail_assert_not_type(integer, Term)).
assert_not_type(Term, number) :- call_protected(\+ number(Term), fail_assert_not_type(number, Term)).
assert_not_type(Term, atom) :- call_protected(\+ atom(Term), fail_assert_not_type(atom, Term)).
assert_not_type(Term, compound) :- call_protected(\+ compound(Term), fail_assert_not_type(compound, Term)).
assert_not_type(Term, list) :- call_protected(\+ is_list(Term), fail_assert_not_type(list, Term)).
assert_not_type(Term, dict) :- call_protected(\+ is_dict(Term), fail_assert_not_type(dict, Term)).

fail_assert_not_type(Expected, Term) :-
    feedback('Asserted ~w is not of type \'~w\', but it is', [Term, Expected]).

%! assert_gt(+A, +B) is semidet
%
% Test that A is greater than B
%
% @arg A
% @arg B
assert_gt(A, B) :-
    call_protected(A > B, fail_assert_gt(A, B)).

fail_assert_gt(A, B) :-
    feedback('Does not hold: ~w is not greater than ~w', [A, B]).

%! assert_lt(+A, +B) is semidet
%
% Test that A is less than B
%
% @arg A
% @arg B
assert_lt(A, B) :-
    call_protected(A < B, fail_assert_lt(A, B)).

fail_assert_lt(A, B) :-
    feedback('Does not hold: ~w is not less than than ~w', [A, B]).

%! assert_gte(+A, +B) is semidet
%
% Test that A is greater than or equal to B
%
% @arg A
% @arg B
assert_gte(A, B) :-
    call_protected(A >= B, fail_assert_gte(A, B)).

fail_assert_gte(A, B) :-
    feedback('Does not hold: ~w is not greater than or equal to ~w', [A, B]).

%! assert_lte(+A, +B) is semidet
%
% Test that A is less than or equal to B
%
% @arg A
% @arg B
assert_lte(A, B) :-
    call_protected(A =< B, fail_assert_lte(A, B)).

fail_assert_lte(A, B) :-
    feedback('Does not hold: ~w is not less than or equal to ~w', [A, B]).


%! assert_output(:Goal, +Vars:list, +Expected:list) is semidet
%
% Test that a predicate's output arguments match what is expected
%
% @arg Goal The predicate to be invoked
% @arg Got The list of vars to be inspected
% @arg Expected The expected values for Vars
assert_output(Goal, Vars, Expected) :-
    call(Goal),
    find_values(Vars, Actual),
    call_protected(Actual == Expected, fail_assert_output(Expected, Actual)),
    !.

find_values([], []).
find_values([V|Vs], [Val|Vals]) :-
    Val = V,
    find_values(Vs, Vals).

fail_assert_output(Expected, Actual) :-
    feedback('Output does not match expected: expected ~w, got ~w', [Expected, Actual]).

% TODO: report on individual vars
% compare_vars([], []) :- !.
% compare_vars([V|Vs], [E|Es]) :-
%     (   V == E
%     ->  true
%     ;   throw(error(pa_assertion_failed(V, E), _))
%     ),
%     compare_vars(Vs, Es).


% private predicates ----------------------------------------------------------


term_type(Term, Type) :-
    (   var(Term) -> Type = variable
    ;   atom(Term) -> Type = atom
    ;   integer(Term) -> Type = integer
    ;   float(Term) -> Type = float
    ;   compound(Term) -> Type = compound
    ;   string(Term) -> Type = string
    ;   Type = unknown
    ).

feedback(Format, Args) :-
    format(atom(Atom), Format, Args),
    format(user_error, '[plunit_assert] ~s', [Atom]).

call_protected(Cond, Callback) :-
    setup_call_cleanup(
        (asserta((prolog:assertion_failed(_, _) :-
                    nb_setval(at_assertion_failed_val, true),
                    call(Callback)),
                Ref),
         nb_setval(at_assertion_failed_val, false)
        ),
        (catch(assertion(Cond), _, true),
         nb_getval(at_assertion_failed_val, Failed)
         ),
        erase(Ref)
    ),
    Failed == false.


% meta-tests ------------------------------------------------------------------


%! assert_test_fails(:Goal) is semidet
%
% Meta test to check that Goal would trigger a PlUnit test fail
%
% @arg Goal The goal to be queried in the form of a plunit_assert predicate
assert_test_fails(Goal) :-
    (   Goal
    ->  feedback('Asserted test failure but test passed: ~q', [Goal]),
        fail
    ;   true
    ).

%! assert_test_passes(:Goal) is semidet
%
% Meta test to check that Goal would not trigger a PlUnit test fail
%
% @arg Goal The goal to be queried in the form of a plunit_assert predicate
assert_test_passes(Goal) :-
    Goal.

% These don't work. See #20

% assert_test_feedback(TestGoal, Expected) :-
%     with_output_to(atom(Actual), catch(TestGoal, _, true)),
%     assert_equals(Actual, Expected).

% assert_test_feedback(TestGoal, Expected) :-
%     current_output(OldOut),
%     with_output_to(atom(Actual), (
%         set_output(user_error),
%         catch(TestGoal, _, true),
%         flush_output(user_error)
%     )),
%     set_output(OldOut),
%     assert_equals(Expected, Actual).
