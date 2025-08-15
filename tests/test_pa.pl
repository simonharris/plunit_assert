:- use_module('../plunit_assert').
:- multifile prolog:assertion_failed/2.

:- begin_tests(pa1).

test(meta_tests) :-
    assert_test_passes(assert_test_fails(assert_true(false))),
    assert_test_fails(assert_test_passes(assert_true(false))),
    assert_test_passes(assert_test_passes(assert_true(true))),
    assert_test_fails(assert_test_fails(assert_true(true))),
    !.

test(pa_truth) :-
     assert_test_passes(assert_true(true)),
     assert_test_fails(assert_true(false)),
     assert_test_passes(assert_false(false)),
     assert_test_fails(assert_false(true)),
    !.

test(pa_equals) :-
    assert_test_passes(assert_equals(3, 3)),
    assert_test_passes(assert_equals(dog, dog)),
    assert_test_passes(assert_equals(14, 7*2)),
    assert_test_fails(assert_equals(7, 6)),
    assert_test_fails(assert_equals(dog, cat)),
   !.

test(pa_not_equals) :- assert_not_equals(3, 4).
test(pa_not_equals) :- assert_not_equals(dog, cat).
test(pa_not_equals) :- assert_test_passes(assert_not_equals(3, 4)).
test(pa_not_equals) :- assert_test_passes(assert_not_equals(dog, cat)).
test(pa_not_equals) :- assert_test_fails(assert_not_equals(7, 7)).
test(pa_not_equals) :- assert_test_fails(assert_not_equals(dog, dog)).
test(pa_not_equals_bug_21) :- assert_test_fails(assert_not_equals(9, 6+3)).

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

test(pa_unbound) :- assert_test_passes(assert_unbound(_)).
test(pa_unbound) :- A = 1, assert_test_fails(assert_unbound(A)).
test(pa_unbound) :- assert_test_fails(assert_unbound(cat)).

test(pa_not_unbound) :- A = 1, assert_test_passes(assert_not_unbound(A)).
test(pa_not_unbound) :- assert_test_passes(assert_not_unbound(1)).
test(pa_not_unbound) :- assert_test_fails(assert_not_unbound(_)).

test(pa_in) :-
    % in list
    assert_in(1, [1, 2, 3]),
    assert_test_fails(assert_in(4, [1, 2, 3])),
    % in dict
    assert_in(a, dict{a:1,b:2,c:3}),
    assert_test_fails(assert_in(d, dict{a:1,b:2,c:3})),
    % in set (just a special example of list)
    List = [1, 1, 2, 3],
    sort(List, Sortedset),
    assert_in(1, Sortedset),
    assert_test_fails(assert_in(4, Sortedset)),
    !.

test(pa_not_in) :-
    % not in list
    assert_not_in(7, [1, 2, 3]),
    assert_test_fails(assert_not_in(3, [1, 2, 3])),
    % not in dict
    assert_not_in(d, dict{a:1,b:2,c:3}),
    assert_test_fails(assert_not_in(a, dict{a:1,b:2,c:3})),
    % in set (just a special example of list)
    List = [1, 1, 2, 3],
    sort(List, Sortedset),
    assert_not_in(7, Sortedset),
    assert_test_fails(assert_not_in(3, Sortedset)),
    !.

test(pa_is_float) :-
    assert_test_passes(assert_type(3.6, float)),
    assert_test_fails(assert_type(2, float)),
    !.

test(pa_is_integer) :-
    assert_test_passes(assert_type(3, integer)),
    assert_test_fails(assert_type(2.0, integer)),
    assert_test_fails(assert_type(bar, integer)),
    !.

test(pa_is_number) :-
    assert_test_passes(assert_type(3, number)),
    assert_test_passes(assert_type(3.6, number)),
    assert_test_fails(assert_type('hello world', number)),
    !.

test(pa_is_atom) :-
    assert_test_passes(assert_type(sup, atom)),
    assert_test_fails(assert_type(6, atom)),
    assert_test_fails(assert_type(_X, atom)),
    !.

test(pa_is_compound) :-
    assert_test_passes(assert_type(foo(bar), compound)),
    assert_test_fails(assert_type(foo, compound)),
    assert_test_fails(assert_type(a, compound)),
    assert_test_fails(assert_type(_Var, compound)),
    !.

test(pa_is_list) :-
    assert_test_passes(assert_type([], list)),
    assert_test_passes(assert_type([foo, bar], list)),
    assert_test_fails(assert_type(foo(bar), list)),
    assert_test_fails(assert_type(foo, list)),
    assert_test_fails(assert_type(a, list)),
    assert_test_fails(assert_type(_Var, list)),
    !.

test(pa_is_dict) :-
    assert_test_passes(assert_type(_{}, dict)),
    assert_test_passes(assert_type(foo{bar:3}, dict)),
    assert_test_fails(assert_type(foo(bar), dict)),
    assert_test_fails(assert_type(foo, dict)),
    assert_test_fails(assert_type(a, dict)),
    assert_test_fails(assert_type(_Var, dict)),
    !.

test(pa_not_type) :-
    assert_test_passes(assert_not_type(3, list)),
    assert_test_passes(assert_not_type(3, atom)),
    assert_test_passes(assert_not_type(3, float)),
    assert_test_passes(assert_not_type(3.0, integer)),
    assert_test_passes(assert_not_type(cow, number)),
    assert_test_passes(assert_not_type(3.0, dict)),
    assert_test_passes(assert_not_type(3.0, compound)),
    assert_test_fails(assert_not_type([], list)),
    assert_test_fails(assert_not_type(cow, atom)),
    assert_test_fails(assert_not_type(3.0, float)),
    assert_test_fails(assert_not_type(9, integer)),
    assert_test_fails(assert_not_type(3.142, number)),
    assert_test_fails(assert_not_type(_{}, dict)),
    assert_test_fails(assert_not_type(horse(dobbin), compound)),
    !.

test(pa_gt) :-
    assert_test_passes(assert_gt(9, 3)),
    assert_test_passes(assert_gt(9, 1+2)),
    assert_test_passes(assert_gt(9, 3.0)),
    assert_test_fails(assert_gt(9, 9)),
    assert_test_fails(assert_gt(3, 9.2)),
    assert_test_fails(assert_gt(3, 8)),
    !.

test(pa_lt) :-
    assert_test_fails(assert_lt(9, 3)),
    assert_test_fails(assert_lt(9, 1+2)),
    assert_test_fails(assert_lt(9, 3.0)),
    assert_test_fails(assert_lt(9, 9)),
    assert_test_passes(assert_lt(3, 9.2)),
    assert_test_passes(assert_lt(3, 8)),
    !.

test(pa_gte) :-
    assert_test_passes(assert_gte(9, 3)),
    assert_test_passes(assert_gte(9, 1+2)),
    assert_test_passes(assert_gte(9, 3.0)),
    assert_test_passes(assert_gte(9, 9)),
    assert_test_fails(assert_gte(3, 9.2)),
    assert_test_fails(assert_gte(3, 8)),
    !.

test(pa_lte) :-
    assert_test_fails(assert_lte(9, 3)),
    assert_test_fails(assert_lte(9, 1+2)),
    assert_test_fails(assert_lte(9, 3.0)),
    assert_test_passes(assert_lte(9, 9)),
    assert_test_passes(assert_lte(3, 9.2)),
    assert_test_passes(assert_lte(3, 8)),
    !.

test(pa_output) :-
    assert_test_passes(assert_output(plus(1, 2, Sum), [Sum], [3])),
    assert_test_passes(assert_output(plus(1, What, 3), [What], [2])),
    assert_test_fails(assert_output(plus(1, 2, Sum), [Sum], [4])),
    assert_test_passes(assert_output(divmod(19, 4, Quotient, Remainder), [Quotient, Remainder], [4, 3])),
    assert_test_fails(assert_output(divmod(19, 4, Quotient, Remainder), [Quotient, Remainder], [14, 3])),
    !.

:- end_tests(pa1).
