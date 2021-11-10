
:- object(tests,
	extends(lgtunit)).

	test(toy_01, true(Fluents == [(location(chris)=work), (rich(chris)=true), (happy(chris)=true)])) :-
		findall(Fluent, {holdsAt(Fluent,16)}, Fluents).

	test(toy_02, true(Pairs == [home-[(22,inf)], pub-[(18,22)], work-[(10,18)]])) :-
		findall(Where-Intervals, {holdsFor((location(chris)=Where), Intervals)}, Pairs).

	test(toy_03, true(Pairs == [true-[(14,20)], false-[]])) :-
		findall(Boolean-Intervals, {holdsFor((rich(chris)=Boolean), Intervals)}, Pairs).

	test(toy_04, true(Pairs == [true-[(14,22)], false-[]])) :-
		findall(Boolean-Intervals, {holdsFor((happy(chris)=Boolean), Intervals)}, Pairs).

	test(toy_05, true(Intervals == [(14,22)])) :-
		{holdsFor(happy(chris)=true, Intervals)}.

:- end_object.
