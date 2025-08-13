:- module(plunit_assert, [
    assert_equals/2,
    assert_not_equals/2,
    assert_gt/2,
    assert_gte/2,
    assert_lt/2,
    assert_lte/2,
    assert_is/2,
    assert_is_not/2,
    assert_exception/1,
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
    assert_test_fails/1,
    assert_test_passes/1
]).
/** <module> The test API for plunit_assert

A unit testing library for Prolog, providing an expressive xUnit-like API for PlUnit.

@author Simon Harding <github@pointbeing.net>
@license MIT
*/
:- dynamic prolog:assertion_failed/2.


%! assert_true(:Goal) is semidet
%
% Test that Goal succeeds and therefore is truthy
%
% @arg Goal The goal to be tested
% @see assertion/1
assert_true(Goal) :-
    assertion(Goal).

%! assert_false(:Goal) is semidet
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
    assertion(A == B).

%! assert_is_not(+A, +B) is semidet
%
% Test that A and B are not identical terms
%
% @arg A The first of the terms to be compared
% @arg B The second of the terms to be compared
% @see assert_is/2
assert_is_not(A, B) :-
    assertion(A \== B).

%! assert_exception(:Goal) is semidet
%
% Test that an exception is thrown during the invocation of Goal
%
% @arg Goal The goal to be tested
% @see assertion/1
assert_exception(Goal) :-
    setup_call_cleanup(
        nb_setval(got_exception, false),
        catch(Goal, _, nb_setval(got_exception, true)),
        true
    ),
    nb_getval(got_exception, Gotex),
    ( Gotex -> true; assertion(false) ).

%! assert_unbound(+Var) is semidet
%
% Test that Var is unbound
%
% This is analogous to isNull() or isNone() in other xUnit implementations
%
% @arg Var The variable to be tested for boundness
% @see assertion/1
assert_unbound(Var) :-
    assertion(var(Var)).

%! assert_not_unbound(+Var) is semidet
%
% Test that Var is not unbound
%
% @arg Var The variable to be tested for unboundness
% @see assert_unbound/1
assert_not_unbound(Var) :-
    assertion(\+ var(Var)).


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
    assertion((
        member(Var, Collection) ;
        get_dict(Var, Collection, _)
    )).

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
    assertion(\+ (
        member(Var, Collection) ;
        ( is_dict(Collection), get_dict(Var, Collection, _) )
    )).

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
assert_type(Term, float) :- assertion(float(Term)).
assert_type(Term, integer) :- assertion(integer(Term)).
assert_type(Term, number) :- assertion(number(Term)).
assert_type(Term, atom) :- assertion(atom(Term)).
assert_type(Term, compound) :- assertion(compound(Term)).
assert_type(Term, list) :- assertion(is_list(Term)).
assert_type(Term, dict) :- assertion(is_dict(Term)).

%! assert_not_type(+Term, +Type) is semidet
%
% Test that Var is not of type Type
%
% @arg Term The term to be tested
% @arg Type The type to be un-asserted
% @see assert_type/2
assert_not_type(Term, float) :- assertion(\+ float(Term)).
assert_not_type(Term, integer) :- assertion(\+ integer(Term)).
assert_not_type(Term, number) :- assertion(\+ number(Term)).
assert_not_type(Term, atom) :- assertion(\+ atom(Term)).
assert_not_type(Term, compound) :- assertion(\+ compound(Term)).
assert_not_type(Term, list) :- assertion(\+ is_list(Term)).
assert_not_type(Term, dict) :- assertion(\+ is_dict(Term)).

%! assert_gt(+A, +B) is semidet
%
% Test that A is greater than B
%
% @arg A
% @arg B
assert_gt(A, B) :-
    assertion(A > B).

%! assert_lt(+A, +B) is semidet
%
% Test that A is less than B
%
% @arg A
% @arg B
assert_lt(A, B) :-
    assertion(A < B).

%! assert_gte(+A, +B) is semidet
%
% Test that A is greater than or equal to B
%
% @arg A
% @arg B
assert_gte(A, B) :-
    assertion(A >= B).

%! assert_lte(+A, +B) is semidet
%
% Test that A is less than or equal to B
%
% @arg A
% @arg B
assert_lte(A, B) :-
    assertion(A =< B).

%! assert_output(:Goal, +Vars:list, +Expected:list) is semidet
%
% Test that a predicate's output arguments match what is expected
%
% @arg Goal The predicate to be invoked
% @arg Vars The list of vars to be inspected
% @arg Expected The expected values for Vars
assert_output(Goal, Vars, Expected) :-
    must_be(list, Vars),
    must_be(list, Expected),
    same_length(Vars, Expected),
    once(call(Goal)),  % actually run the predicate
    assertion(Vars == Expected).
    % compare_vars(Vars, Expected).

% TODO: report on individual vars
% compare_vars([], []) :- !.
% compare_vars([V|Vs], [E|Es]) :-
%     (   V == E
%     ->  true
%     ;   throw(error(pa_assertion_failed(V, E), _))
%     ),
%     compare_vars(Vs, Es).


% meta-tests ------------------------------------------------------------------


%! assert_test_fails(:Goal) is semidet
%
% Meta test to check that Goal would trigger a PlUnit test fail
%
% @arg Goal The goal to be queried in the form of a plunit_assert predicate
assert_test_fails(Goal) :-
    setup_call_cleanup(
        (asserta((prolog:assertion_failed(Reason, Somegoal) :-
                    pa_assertion_failed(Reason, Somegoal)),
                Ref),
         nb_setval(assertion_failed, false)
        ),
        (catch(Goal, _, true),
         nb_getval(assertion_failed, Failed)
         ),
        erase(Ref)
    ),
    Failed == true.

pa_assertion_failed(_, _) :-
    nb_setval(assertion_failed, true).

%! assert_test_passes(:Goal) is semidet
%
% Meta test to check that Goal would not trigger a PlUnit test fail
%
% @arg Goal The goal to be queried in the form of a plunit_assert predicate
assert_test_passes(Goal) :-
    Goal.


