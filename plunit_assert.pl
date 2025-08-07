/** <module> The test API for plunit_assert

A unit testing library for Prolog, providing an expressive xUnit-like API for PlUnit.

@author Simon Harding <github@pointbeing.net?
@license MIT
*/
:- module(plunit_assert, [
    assert_equals/2,
    assert_not_equals/2,
    assert_is/2,
    % assert_exception/1,
    assert_false/1,
    assert_true/1,
    assert_unbound/1,
    % Meta stuff - not really part of the plunit_assert API
    assert_test_fails/1,
    assert_test_passes/1
]).
:- dynamic prolog:assertion_failed/2.


%% assert_true(+Goal) is det
%
%  Test that Goal succeeds and therefore is truthy
%
%  @arg Goal The goal to be tested
%  @see assertion/1
assert_true(Goal) :-
    assertion(Goal).

%% assert_false(+Goal) is det
%
%  Test that Goal fails and therefore is falsy 3
%
%  @arg Goal The goal to be tested
%  @see assertion/1
assert_false(Goal) :-
    assertion(\+ Goal).

assert_equals(A, B) :-
    assertion(A == B; A =:= B).

assert_is(A, B) :-
    assertion(A == B).

assert_not_equals(A, B) :-
    assertion(A \= B).

assert_exception(Goal) :-
    catch(Goal, _, true),
    !.


%% assert_unbound(+Var) is det
%
%  Test that Var is unbound
%
%  This is analogous to isNul() or isNone() in other xUnit implementations
%
%  @arg Goal The goal to be tested
%  @see assertion/1
assert_unbound(Var) :-
    assertion(var(Var)).

% meta-meta-tests -------------------------------------------------------------


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

assert_test_passes(Goal) :-
    Goal.

pa_assertion_failed(_, _) :-
    %writeln('Captured test fail'),
    !.



