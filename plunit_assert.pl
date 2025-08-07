:- module(plunit_assert, [
    assert_equals/2,
    assert_not_equals/2,
    assert_is/2,
    assert_is_not/2,
    % assert_exception/1,
    assert_false/1,
    assert_true/1,
    assert_unbound/1,
    assert_not_unbound/1,
    % Meta stuff - not really part of the plunit_assert API
    assert_test_fails/1,
    assert_test_passes/1
]).
/** <module> The test API for plunit_assert

A unit testing library for Prolog, providing an expressive xUnit-like API for PlUnit.

@author Simon Harding <github@pointbeing.net?
@license MIT
*/
:- dynamic prolog:assertion_failed/2.


%% assert_true(+Goal) is semidet
%
% Test that Goal succeeds and therefore is truthy
%
% @arg Goal The goal to be tested
% @see assertion/1
assert_true(Goal) :-
    assertion(Goal).

%% assert_false(+Goal) is semidet
%
% Test that Goal fails and therefore is falsy
%
% @arg Goal The goal to be tested
% @see assertion/1
assert_false(Goal) :-
    assertion(\+ Goal).

%! assert_equals(+A, +B) is semidet
%
% This is a superset of assert_is/2 and arithmetic comparison with =:=
%
% @arg A The first of the terms to be compared
% @arg B The second of the terms to be compared
assert_equals(A, B) :-
    assertion(A == B; A =:= B).

%! assert_not_equals(+A, +B) is semidet
%
% Test that A and B are not equal terms
%
% @arg A The first of the terms to be compared
% @arg B The second of the terms to be compared
% @see assert_equals/2
assert_not_equals(A, B) :-
    assertion(A \= B).

%% assert_is(+A, +B) is semidet
%
% Test that A and B are identical terms
%
% Use ==/2 to check for term identity, which means it compares the terms A and
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
    assertion(A == B).


%% assert_is_not(+A, +B) is semidet
%
% Test that A and B are not identical terms
%
% @arg A The first of the terms to be compared
% @arg B The second of the terms to be compared
% @see assert_is/2
assert_is_not(A, B) :-
    assertion(A \== B).


% assert_exception(Goal) :-
%     catch(Goal, _, true),
%     !.


%! assert_unbound(+Var) is semidet
%
%  Test that Var is unbound
%
% This is analogous to isNul() or isNone() in other xUnit implementations
%
% @arg Var The variable to be tested for boundness
% @see assertion/1
assert_unbound(Var) :-
    assertion(var(Var)).

%! assert_not_unbound(+Var) is semidet
%
%  Test that Var is not unbound
%
% @arg Var The variable to be tested for unboundness
% @see assert_unbound/1
assert_not_unbound(Var) :-
    assertion(\+ var(Var)).


% meta-tests ------------------------------------------------------------------


%! assert_test_fails(+Goal) is semidet
%
% Meta test to check that Goal would trigger a PlUnit test fail
%
% @arg Goal The goal to be queried in the form of a plunit_assert predicate
assert_test_fails(Goal) :-
    setup_call_cleanup(
        asserta((prolog:assertion_failed(Reason, Somegoal) :-
                    pa_assertion_failed(Reason, Somegoal),
                    nb_setval(assertion_failed, true)),
                Ref),
        (nb_setval(assertion_failed, false),
         catch(Goal, _, true),
         nb_getval(assertion_failed, Failed)),
        erase(Ref)
    ),
    Failed == true.

%! assert_test_passes(+Goal) is semidet
%
% Meta test to check that Goal would not trigger a PlUnit test fail
%
% @arg Goal The goal to be queried in the form of a plunit_assert predicate
assert_test_passes(Goal) :-
    Goal.

pa_assertion_failed(_, _) :-
    %writeln('Captured test fail'),
    !.
