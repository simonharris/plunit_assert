:- ensure_loaded('../plunit_assert').


:- begin_tests(pa1).


test(a_truth) :-
    assert_true(assert_true(true)),
    assert_false(assert_true(false)),
    assert_true(assert_false(false)),
    assert_false(assert_false(true)),
    !.


test(a_equals) :-
    assert_equals(3, 3),
    assert_equals(dog, dog).


:- end_tests(pa1).
