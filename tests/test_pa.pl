:- ensure_loaded('../plunit_assert').


:- begin_tests(pa1).

% Won't work yet. See https://github.com/simonharris/plunit_assert/issues/6
% test(meta_tests) :-
%     assert_test_passes(assert_test_fails(assert_true(false))),
%     assert_test_fails(assert_test_passes(assert_true(false))),
%     assert_test_passes(assert_test_passes(assert_true(true))),
%     assert_test_fails(assert_test_fails(assert_true(true))),
%     !.

% test(pa_except) :-
%     assert_exception(throw(pa_exception)),
%     assert_exception(assert_exception(true)),
%     !.

test(pa_truth) :-
    assert_test_passes(assert_true(true)),
    assert_test_fails(assert_true(false)),
    assert_test_passes(assert_false(false)),
    assert_test_fails(assert_false(true)),
    !.

test(pa_equals) :-
    assert_equals(3, 3),
    assert_equals(dog, dog),
    assert_test_fails(assert_equals(7, 6)),
    assert_test_fails(assert_equals(dog, cat)),
   !.

test(pa_not_equals) :-
    assert_not_equals(3, 4),
    assert_not_equals(dog, cat),
    assert_test_passes(assert_not_equals(3, 4)),
    assert_test_passes(assert_not_equals(dog, cat)),
    assert_test_fails(assert_not_equals(7, 7)),
    assert_test_fails(assert_not_equals(dog, dog)),
    !.

:- end_tests(pa1).
