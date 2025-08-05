:- module(plunit_assert, [
    assert_equals/2,
    assert_exception/1,
    assert_false/1,
    assert_true/1,
    % Meta stuff - not really part of the plunit_assert API
    assert_test_fails/1,
    assert_test_passes/1
]).


assert_true(Goal) :-
    assertion(Goal).

assert_false(Goal) :-
    assertion(\+ Goal).

assert_equals(A, B) :-
    assertion(A == B).

assert_exception(Goal) :-
    catch(Goal, _, true),
    !.

assert_test_fails(Goal) :-
    asserta((prolog:assertion_failed(Reason, Somegoal) :-
		    pa_assertion_failed(Reason, Somegoal)),
	    Ref),
    Goal,
    erase(Ref).

assert_test_passes(Goal) :-
    Goal.

pa_assertion_failed(_, _) :-
    % =format(user_error, '~w~n', 'Assertion failed and caught!'),
    true.
