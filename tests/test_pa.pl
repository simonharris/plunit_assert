:- ensure_loaded('../plunit_assert').


:- begin_tests(pa1).

% Won't work yet. See https://github.com/simonharris/plunit_assert/issues/6
% test(meta_tests) :-
%     assert_test_passes(assert_test_fails(assert_true(false))),
%     assert_test_fails(assert_test_passes(assert_true(false))),
%     assert_test_passes(assert_test_passes(assert_true(true))),
%     assert_test_fails(assert_test_fails(assert_true(true))),
%     !.

test(pa_exception) :-
    assert_exception(throw(pa_exception)),
    assert_test_passes(assert_exception(throw(pa_exception))),
    assert_test_fails(assert_exception(true)),
    !.

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

test(pa_identity) :-
    assert_is(foo(cat, 3), foo(cat, 3)),
    assert_test_fails(assert_is(foo(dog, 4), foo(cat, 3))),
    assert_test_fails(assert_is(dog, cat)),
    assert_test_passes(assert_is(dog, dog)),
    assert_test_passes(assert_is(B, B)),
    !.

test(pa_not_identity) :-
    assert_test_passes(assert_is_not(1, 2)),
    assert_test_passes(assert_is_not(cat, dog)),
    assert_test_passes(assert_is_not(foo(cat, 3), foo(cat, 4))),
    assert_test_fails(assert_is_not(3, 3)),
    assert_test_fails(assert_is_not(cat, cat)),
    assert_test_fails(assert_is_not(foo(cat, 3), foo(cat, 3))),
    !.

test(pa_identity_vs_equality) :-
    A = 1 + 2,
    B = 3,
    assert_test_passes(assert_equals(A, B)),
    assert_test_fails(assert_is(A, B)),
    !.

test(pa_unbound) :-
    assert_test_passes(assert_unbound(_)),
    A = 1,
    assert_test_fails(assert_unbound(A)),
    assert_test_fails(assert_unbound(cat)),
    !.

test(pa_not_unbound) :-

    A = 1,
    assert_test_passes(assert_not_unbound(A)),
    assert_test_passes(assert_not_unbound(1)),
    assert_test_fails(assert_not_unbound(_)),
    !.

:- end_tests(pa1).
