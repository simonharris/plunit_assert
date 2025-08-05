:- ensure_loaded('../plunit_assert').


:- begin_tests(pa1).

test(meta_pass) :-
    assert_test_passes(assert_test_fails(assert_true(false))),
    assert_test_fails(assert_test_passes(assert_true(false))),
    assert_test_passes(assert_test_passes(assert_true(true))),
    assert_test_fails(assert_test_fails(assert_true(true))),
    !.

test(a_except) :-
    assert_exception(throw(pa_exception)),
    assert_exception(assert_exception(true)),
    !.

test(a_truth) :-
    assert_test_passes(assert_true(true)),
    assert_test_fails(assert_true(false)),
    assert_test_passes(assert_false(false)),
    assert_test_fails(assert_false(true)),
    !.

test(a_equals) :-
    assert_equals(3, 3),
    assert_equals(dog, dog),
    assert_test_fails(assert_equals(7, 6)),
    assert_test_fails(assert_equals(dog, cat)),
    !.

:- end_tests(pa1).
