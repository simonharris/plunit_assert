:- module(plunit_assert, [
    assert_equals/2,
    assert_exception/1,
    assert_false/1,
    assert_true/1
]).


assert_true(Val) :-
    Val.


assert_false(Val) :-
    \+ Val.


assert_equals(A, B) :-
    assertion(A == B).


assert_exception(Goal) :-
    catch(Goal, _, true),
    !.
assert_exception(_, _) :-
    throw(error(assertion_error, 'No exception was thrown')).

