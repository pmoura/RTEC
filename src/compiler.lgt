
/***************************************************************************************************
 This program compiles an event description into a more efficient representation.
 It also compiles some types of declaration into a format that allows for more efficient reasoning.
 Input:
 (a) Event Calculus axioms.
 (b) Declarations.

 Event processing should be performed on the event description
 produced by this compiler, along with the declarations.
 ***************************************************************************************************/

:- object(compiler(_Kind_),
	implements(expanding)).

	% compiler predicates cache
	:- private([
		index_/2, buildFromPoints_/1, collectIntervals_/1, cachingOrder_/1, grounding_/1
	]).
	:- dynamic([
		index_/2, buildFromPoints_/1, collectIntervals_/1, cachingOrder_/1, grounding_/1
	]).

	:- uses(list, [
		append/3
	]).

	:- uses(user, [
		cachingOrder/1, index/2, simpleFluent/1, sDFluent/1,
		inputEntity/1, outputEntity/1, internalEntity/1, cyclic/1
	]).

	% these predicates are defined in this file
	:- discontiguous([
		compileHoldsAtTree/3, findChildren/3
	]).

	indexOf(Index, Entity) :-
		index_(Entity, Index).

    term_expansion(begin_of_file, begin_of_file) :-
    	_Kind_ == declarations,
    	retractall(index_(_, _)),
    	retractall(buildFromPoints_(_)),
    	retractall(collectIntervals_(_)),
    	retractall(cachingOrder_(_)),
    	retractall(grounding_(_)).

    % compile initially/1 rules
    term_expansion((initially(F=V) :- Body), Clause) :-
    	nonvar(F),
    	T = -1,
    	(
    		Body = (true),
    		BodyWithTimeSpan = (T1=<T, T<T2)
    		;
    		Body \= (true),
    		BodyWithTimeSpan = (Body,T1=<T, T<T2)
    	),
    	compileConditions(BodyWithTimeSpan, NewBody, [T1, T2], false),
    	Clause = (initiatedAt(F=V,T1,T,T2) :- NewBody).

    % compile initiatedAt/2 rules
    term_expansion((initiatedAt(F=V,T) :- Body), Clause) :-
    	nonvar(F),
    	(
    			cyclic(F=V),
    			compileConditions(Body, NewBody, [T1, T2], true)
    			;
    			\+ cyclic(F=V),
    			compileConditions(Body, NewBody, [T1, T2], false)
    	),
    	Clause = (initiatedAt(F=V,T1,T,T2) :- NewBody).

    % compile initiatedAt/4 rules
    % In this case, we assume the author treats timespans correctly inside the rule body
    term_expansion((initiatedAt(F=V,T1,T,T2) :- Body), Clause) :-
    	nonvar(F),
    	(
    		cyclic(F=V),
    			compileConditions(Body, NewBody, [], true)
    			;
    			\+ cyclic(F=V),
    			compileConditions(Body, NewBody, [], false)
    	),
    	Clause = (initiatedAt(F=V,T1,T,T2) :- NewBody).

    % compile terminatedAt/2 rules
    term_expansion((terminatedAt(F=V,T) :- Body), Clause) :-
    	nonvar(F),
    	(
    		cyclic(F=V),
    		compileConditions(Body, NewBody, [T1, T2], true)
    		;
    		\+ cyclic(F=V),
    		compileConditions(Body, NewBody, [T1, T2], false)
    	),
    	Clause = (terminatedAt(F=V,T1,T,T2) :- NewBody).

    % compile terminatedAt/4 rules
    % In this case, we assume the author treats timespans correctly inside the rule body
    term_expansion((terminatedAt(F=V,T1,T,T2), Body), Clause) :-
    	nonvar(F),
    	(
    		cyclic(F=V),
    		compileConditions(Body, NewBody, [], true)
    	;
    		\+ cyclic(F=V),
    		compileConditions(Body, NewBody, [], false)
    	),
    	Clause = (terminatedAt(F=V,T1,T,T2) :- NewBody).

    % compile initiates/3 rules
    term_expansion((initiates(E,F=V,T) :- Body), Clause) :-
    	nonvar(F),
    	(
    		cyclic(F=V),
    		compileConditions((happensAt(E,T),Body), NewBody, [T1, T2], true)
    	;
    		\+ cyclic(F=V),
    		compileConditions((happensAt(E,T),Body), NewBody, [T1, T2], false)
    	),
    	Clause = (initiatedAt(F=V,T1,T,T2) :- NewBody).

    % compile terminates/3 rules
    term_expansion((terminates(E,F=V,T) :- Body), Clause) :-
    	nonvar(F),
    	(
    		cyclic(F=V),
    		compileConditions((happensAt(E,T),Body), NewBody, [T1, T2], true)
    		;
    		\+ cyclic(F=V),
    		compileConditions((happensAt(E,T),Body), NewBody, [T1, T2], false)
    	),
    	Clause = (terminatedAt(F=V,T1,T,T2) :- NewBody).

    % compile holdsFor/2 rules
    term_expansion((holdsFor(F=V,I) :- Body), Clause) :-
    	% the condition below makes sure that we do not compile rules from RTEC.prolog
    	% or any other domain-independent code
    	nonvar(F),
    	compileConditions(Body, NewBody, [], false),
    	Clause = (holdsForSDFluent(F=V,I) :- NewBody).

    % compile holdsAt/2 rules
    term_expansion((holdsAt(F=V,_T) :- Body), Clause) :-
    	% the condition below makes sure that we do not compile rules from RTEC.prolog
    	% or any other domain-independent code
    	nonvar(F),
    	compileHoldsAtTree(Body, NewBody, I),
    	Clause = (holdsFor(F=V,I) :- NewBody).

    % compile happensAt/2 rules
    term_expansion((happensAt(E,T) :- Body), Clause) :-
    	% the condition below makes sure that we do not compile rules from RTEC.prolog
    	% or any other domain-independent code
    	nonvar(E),
    	compileConditions(Body, NewBody, [], false),
    	Clause = (happensAtEv(E,T) :- NewBody).

    % cache grounding/1 clauses
    term_expansion((grounding(Entity) :- Body), (grounding(Entity) :- Body)) :-
    	assertz((grounding_(Entity) :- Body)).
    term_expansion(grounding(Entity), grounding(Entity)) :-
    	assertz(grounding_(Entity)).

    % cache cachingOrder/1 clauses
    term_expansion((cachingOrder(Entity) :- Body), (cachingOrder(Entity) :- Body)) :-
    	assertz((cachingOrder_(Entity) :- Body)).
    term_expansion(cachingOrder(Entity), cachingOrder(Entity)) :-
    	assertz(cachingOrder_(Entity)).

    % cache collectIntervals/1 clauses
    term_expansion((collectIntervals(Interval) :- Body), (collectIntervals(Interval) :- Body)) :-
    	assertz((collectIntervals_(Interval) :- Body)).
    term_expansion(collectIntervals(Interval), collectIntervalsInterval) :-
    	assertz(collectIntervals_(Interval)).

    % cache buildFromPoints/1 clauses
    term_expansion((buildFromPoints(Interval) :- Body), (buildFromPoints(Interval) :- Body)) :-
    	assertz((buildFromPoints_(Interval) :- Body)).
    term_expansion(buildFromPoints(Interval), buildFromPoints) :-
    	assertz(buildFromPoints_(Interval)).

    % cache index/2 clauses
    term_expansion((index(Index, Entity) :- Body), (index(Index, Entity) :- Body)) :-
    	assertz((index_(Index, Entity) :- Body)).
    term_expansion(index(Index, Entity), index(Index, Entity)) :-
    	assertz(index_(Index, Entity)).

    term_expansion(end_of_file, Clauses) :-
    	_Kind_ == rules,
    	findall(Clause, compileCachingOrder(Clause), Clauses0),
    	findall(Clause, compileCollectIntervals(Clause), Clauses1, Clauses0),
    	findall(Clause, compileBuildFromPoints(Clause), Clauses2, Clauses1),
    	append(Clauses2, [end_of_file], Clauses).

    % compile cachingOrder/1 rules
    compileCachingOrder(Clause) :-
    	cachingOrder_(Entity),
    	clause(grounding_(Entity), Body),
    	indexOf(Index, Entity),
    	Clause = (cachingOrder2(Index, Entity) :- Body).

    % compile collectIntervals/1 rules
    compileCollectIntervals(Clause) :-
    	collectIntervals_(F=V),
    	clause(grounding_(F=V), Body),
    	indexOf(Index, F=V),
    	Clause = (collectIntervals2(Index, F=V) :- Body).

    % compile buildFromPoints/1 rules
    compileBuildFromPoints(Clause) :-
    	buildFromPoints_(F=V),
    	clause(grounding_(F=V), Body),
    	indexOf(Index, F=V),
    	Clause = (buildFromPoints2(Index, F=V) :- Body).

%%%%%%%% compile body predicates %%%%%%%%

%%%% recursive definition of compileConditions/4 %%%%

compileConditions((\+Head,Rest), (\+NewHead,NewRest), Timespan, Cyclic) :-
	!, compileConditions1(Head, NewHead, Timespan, Cyclic),
	compileConditions(Rest, NewRest, Timespan, Cyclic).

compileConditions((Head,Rest), (NewHead,NewRest), Timespan, Cyclic) :-
	!, compileConditions1(Head, NewHead, Timespan, Cyclic),
	% below TimeSpan is set to [] since the constraint 'T1=<T,T<2'
	% can only be imposed on the first body literal
	compileConditions(Rest, NewRest, [], Cyclic).

compileConditions(\+Body, \+NewBody, Timespan, Cyclic) :-
	!, compileConditions1(Body, NewBody, Timespan, Cyclic).

compileConditions(Body, NewBody, Timespan, Cyclic) :-
	compileConditions1(Body, NewBody, Timespan, Cyclic).


%%%% recursive definition of compileHoldsAtTree/3 %%%%
compileHoldsAtTree(Body, NewBody, Interval) :-
	findChildren(Body, Children, Operation),
	!,
	/*findall([ChildNewBody,ChildInterval],
			(member(Child,Children),compileHoldsAtTree(Child,ChildNewBody,ChildInterval)),
			ChildrenBIs),*/
	% findall creates new variable bindings. Use gather instead.
	gatherChildrenBodyIntervals(Children,[],ChildrenBIs),
	completeBody(ChildrenBIs,Operation,NewBody,Interval).

gatherChildrenBodyIntervals([HeadChild|[]],InitChildrenBIs,ChildrenBIs) :-
	compileHoldsAtTree(HeadChild,ChildNewBody,ChildInterval),
	append(InitChildrenBIs,[[ChildNewBody,ChildInterval]],ChildrenBIs).

gatherChildrenBodyIntervals([HeadChild|TailChildren],InitChildrenBIs,ChildrenBIs) :-
	compileHoldsAtTree(HeadChild,ChildNewBody,ChildInterval),
	append(InitChildrenBIs,[[ChildNewBody,ChildInterval]],NewInitChildrenBIs),
	gatherChildrenBodyIntervals(TailChildren,NewInitChildrenBIs,ChildrenBIs).

% simple fluent
compileHoldsAtTree(holdsAt(U,_T), rtec::holdsForProcessedSimpleFluent(Index,U,I), I) :-
	simpleFluent(U), indexOf(Index, U), !.

% output entity/statically determined fluent
compileHoldsAtTree(holdsAt(U,_T), rtec::holdsForProcessedSDFluent(Index,U,I), I) :-
	sDFluent(U), indexOf(Index, U), !.

findChildren(Body,Children,Operation) :-
	checkForNegation(Body,Intersections,Unions),
	convertToInters(Intersections,ChildrenI),
	convertToUnions(Unions,ChildrenU),
	Children = [ChildrenI,ChildrenU],
	Operation = negation.

checkForNegation(Body,Intersections,Unions) :-
	checkForNegation1(Body,[],Intersections,[],Unions),
	Unions \= [].

checkForNegation1((\+Head,Rest),InitIntersections,Intersections,InitUnions,Unions) :-
	append(InitUnions,[Head],NewInitUnions),
	checkForNegation1(Rest,InitIntersections,Intersections,NewInitUnions,Unions).

checkForNegation1((Head,Rest),InitIntersections,Intersections,InitUnions,Unions) :-
	append(InitIntersections,[Head],NewInitIntersections),
	checkForNegation1(Rest,NewInitIntersections,Intersections,InitUnions,Unions).

checkForNegation1(\+Body,InitIntersections,InitIntersections,InitUnions,Unions) :-
	append(InitUnions,[Body],Unions).

checkForNegation1(Body,InitIntersections,Intersections,InitUnions,InitUnions) :-
	append(InitIntersections,[Body],Intersections).

convertToInters([H|[]], H).

convertToInters([H|T],(H,Rest)) :-
	convertToInters(T,Rest).

convertToUnions([H|[]], H).

convertToUnions([H|T],(H;Rest)) :-
	convertToUnions(T,Rest).

findChildren((Head,Rest),Children,Operation) :-
	findChildren1((Head,Rest), [], Children, Operation).

findChildren((Head;Rest),Children,Operation) :-
	findChildren1((Head;Rest), [], Children, Operation).

findChildren1((Head,Rest), InitChildren, Children, intersection) :-
	!, append(InitChildren,[Head],NewInitChildren),
	findChildren1(Rest, NewInitChildren, Children, intersection).

findChildren1((Head;Rest), InitChildren, Children, union) :-
	!, append(InitChildren,[Head],NewInitChildren),
	findChildren1(Rest, NewInitChildren, Children, union).

findChildren1(Body, InitChildren, Children, _Operation) :-
	append(InitChildren, [Body], Children).

completeBody(ChildrenBIs,intersection,(Head,Rest),Interval) :-
	completeBody1(ChildrenBIs,Head,[],Intervals),
	Rest = intervals::intersect_all(Intervals, Interval).

completeBody(ChildrenBIs,union,(Head,Rest),Interval) :-
	completeBody1(ChildrenBIs,Head,[],Intervals),
	Rest = intervals::union_all(Intervals, Interval).

completeBody(ChildrenBIs,negation,(Head,Rest),Interval) :-
	completeBody1(ChildrenBIs,Head,[],Intervals),
	Intervals = [H|T],
	Rest = intervals::relative_complement_all(H, T, Interval).

completeBody1([H|[]],(Head),InitIntervals,Intervals) :-
	H = [Head|Interval],
	append(InitIntervals,Interval,Intervals).

completeBody1([H|T],(Head,Rest),InitIntervals,Intervals) :-
	H = [Head|Interval],
	append(InitIntervals,Interval,NewIntervals),
	completeBody1(T,Rest,NewIntervals,Intervals).

%%%% end of recursive definition of compileHoldsAtTree/3 %%%%


%%%% auxiliary predicate dealing with a single condition %%%%

%%% happensAt

% special event: start of simple fluent
compileConditions1(happensAt(start(F=V),T), NewBody, Timespan, _Cyclic) :-
	% we use copy_term in compileConditions1 to classify U without
	% affecting its (free) variables
	copy_term(F=V, U1), simpleFluent(U1),
	% but use U in the compiled event description
	% a free variable is used below to avoid instantiating V,
	% in case it is a variable, to some arbitrary ground value
	indexOf(Index, F=_),
	(
		Timespan = [],
		NewBody = happensAtProcessedSimpleFluent(Index,start(F=V),T)
		;
		Timespan = [T1, T2],
		NewBody = (happensAtProcessedSimpleFluent(Index,start(F=V),T), T1=<T, T<T2)
	), !.

% special event: start of input entity/statically determined fluent
compileConditions1(happensAt(start(F=V),T), NewBody, Timespan, _Cyclic) :-
	copy_term(F=V, U1), sDFluent(U1), inputEntity(U1),
	indexOf(Index, F=_),
	(
		Timespan = [],
		NewBody = happensAtProcessedIE(Index,start(F=V),T)
		;
		Timespan = [T1, T2],
		NewBody = (happensAtProcessedIE(Index,start(F=V),T), T1=<T, T<T2)
	), !.

% special event: start of internal entity/statically determined fluent
compileConditions1(happensAt(start(F=V),T), NewBody, Timespan, _Cyclic) :-
	copy_term(F=V, U1), sDFluent(U1), internalEntity(U1),
	indexOf(Index, F=_),
	(
		Timespan = [],
		NewBody = happensAtProcessedIE(Index,start(F=V),T)
		;
		Timespan = [T1, T2],
		NewBody = (happensAtProcessedIE(Index,start(F=V),T), T1=<T, T<T2)
	), !.

% special event: start of output entity/statically determined fluent
compileConditions1(happensAt(start(F=V),T), NewBody, Timespan, _Cyclic) :-
	copy_term(F=V, U1), sDFluent(U1), outputEntity(U1),
	indexOf(Index, F=_),
	(
		Timespan = [],
		NewBody = happensAtProcessedSDFluent(Index,start(F=V),T)
		;
		Timespan = [T1, T2],
		NewBody = (happensAtProcessedSDFluent(Index,start(F=V),T), T1=<T, T<T2)
	), !.

% ---------------------------------compile startI-------------------------------------
compileConditions1(happensAt(startI(F=V),T), NewBody, Timespan, _Cyclic) :-
	copy_term(F=V, U1), simpleFluent(U1),
	indexOf(Index, F=_),
	(
		Timespan = [],
		NewBody = happensAtProcessedSimpleFluent(Index,startI(F=V),T)
		;
		Timespan = [T1, T2],
		NewBody = (happensAtProcessedSimpleFluent(Index,startI(F=V),T), T1=<T, T<T2)
	), !.

% special event: start of input entity/statically determined fluent
compileConditions1(happensAt(startI(F=V),T), NewBody, Timespan, _Cyclic) :-
	copy_term(F=V, U1), sDFluent(U1), inputEntity(U1),
	indexOf(Index, F=_),
	(
		Timespan = [],
		NewBody = happensAtProcessedIE(Index,startI(F=V),T)
		;
		Timespan = [T1, T2],
		NewBody = (happensAtProcessedIE(Index,startI(F=V),T), T1=<T, T<T2)
	), !.

% special event: start of internal entity/statically determined fluent
compileConditions1(happensAt(startI(F=V),T), NewBody, Timespan, _Cyclic) :-
	copy_term(F=V, U1), sDFluent(U1), internalEntity(U1),
	indexOf(Index, F=_),
	(
		Timespan = [],
		NewBody = happensAtProcessedIE(Index,startI(F=V),T)
		;
		Timespan = [T1, T2],
		NewBody = (happensAtProcessedIE(Index,startI(F=V),T), T1=<T, T<T2)
	), !.

% special event: start of output entity/statically determined fluent
compileConditions1(happensAt(startI(F=V),T), NewBody, Timespan, _Cyclic) :-
	copy_term(F=V, U1), sDFluent(U1), outputEntity(U1),
	indexOf(Index, F=_),
	(
		Timespan = [],
		NewBody = happensAtProcessedSDFluent(Index,startI(F=V),T)
		;
		Timespan = [T1, T2],
		NewBody = (happensAtProcessedSDFluent(Index,startI(F=V),T), T1=<T, T<T2)
	), !.
% ---------------------------------compile startI-------------------------------------

% special event: end of simple fluent
compileConditions1(happensAt(end(F=V),T), NewBody, Timespan, _Cyclic) :-
	copy_term(F=V, U1), simpleFluent(U1),
	indexOf(Index, F=_),
	(
		Timespan = [],
		NewBody = happensAtProcessedSimpleFluent(Index,end(F=V),T)
		;
		Timespan = [T1, T2],
		NewBody = (happensAtProcessedSimpleFluent(Index,end(F=V),T), T1=<T, T<T2)
	), !.

% special event: end of input entity/statically determined fluent
compileConditions1(happensAt(end(F=V),T), NewBody, Timespan, _Cyclic) :-
	copy_term(F=V, U1), sDFluent(U1), inputEntity(U1),
	indexOf(Index, F=_),
	(
		Timespan = [],
		NewBody = happensAtProcessedIE(Index,end(F=V),T)
		;
		Timespan = [T1, T2],
		NewBody = (happensAtProcessedIE(Index,end(F=V),T), T1=<T, T<T2)
	), !.

% special event: end of internal entity/statically determined fluent
compileConditions1(happensAt(end(F=V),T), NewBody, Timespan, _Cyclic) :-
	copy_term(F=V, U1), sDFluent(U1), internalEntity(U1),
	indexOf(Index, F=_),
	(
		Timespan = [],
		NewBody = happensAtProcessedIE(Index,end(F=V),T)
		;
		Timespan = [T1, T2],
		NewBody = (happensAtProcessedIE(Index,end(F=V),T), T1=<T, T<T2)
	), !.

% special event: end of output entity/statically determined fluent
compileConditions1(happensAt(end(F=V),T), NewBody, Timespan, _Cyclic) :-
	copy_term(F=V, U1), sDFluent(U1), outputEntity(U1),
	indexOf(Index, F=_),
	(
		Timespan = [],
		NewBody = happensAtProcessedSDFluent(Index,end(F=V),T)
		;
		Timespan = [T1, T2],
		NewBody = (happensAtProcessedSDFluent(Index,end(F=V),T), T1=<T, T<T2)
	), !.

% special event: end of statically determined fluent that is neither an input nor an output entity
compileConditions1(happensAt(end(F=V),T), NewBody, Timespan, _Cyclic) :-
	copy_term(F=V, U1), sDFluent(U1),
	(
		Timespan = [],
		NewBody = happensAtSDFluent(end(F=V),T)
		;
		Timespan = [T1, T2],
		NewBody = (happensAtSDFluent(end(F=V),T), T1=<T, T<T2)
	), !.
/*
%---------------------------- compile endI-------------------------------
% special event: end of simple fluent
compileConditions1(happensAt(endI(U),T), NewBody, Timespan, Cyclic) :-
	simpleFluent(U), indexOf(Index, U),
	(
	Timespan = [],
	NewBody = happensAtProcessedSimpleFluent(Index,endI(U),T)
	;
	Timespan = [T1, T2],
	NewBody = (happensAtProcessedSimpleFluent(Index,endI(U),T), T1=<T, T<T2)
	),
	!.

% special event: end of input entity/statically determined fluent
compileConditions1(happensAt(endI(U),T), NewBody, Timespan, Cyclic) :-
	sDFluent(U), inputEntity(U), indexOf(Index, U),
	(
	Timespan = [],
	NewBody = happensAtProcessedIE(Index,endI(U),T)
	;
	Timespan = [T1, T2],
	NewBody = (happensAtProcessedIE(Index,endI(U),T), T1=<T, T<T2)
	),
	!.

% special event: end of internal entity/statically determined fluent
compileConditions1(happensAt(endI(U),T), NewBody, Timespan, Cyclic) :-
	sDFluent(U), internalEntity(U), indexOf(Index, U),
	(
	Timespan = [],
	NewBody = happensAtProcessedIE(Index,endI(U),T)
	;
	Timespan = [T1, T2],
	NewBody = (happensAtProcessedIE(Index,endI(U),T), T1=<T, T<T2)
	),
	!.

% special event: end of output entity/statically determined fluent
compileConditions1(happensAt(endI(U),T), NewBody, Timespan, Cyclic) :-
	sDFluent(U), outputEntity(U), indexOf(Index, U),
	(
	Timespan = [],
	NewBody = happensAtProcessedSDFluent(Index,endI(U),T)
	;
	Timespan = [T1, T2],
	NewBody = (happensAtProcessedSDFluent(Index,endI(U),T), T1=<T, T<T2)
	),
	!.

% special event: end of statically determined fluent that is neither an input nor an output entity
compileConditions1(happensAt(endI(U),T), NewBody, Timespan, Cyclic) :-
	sDFluent(U),
	(
	Timespan = [],
	NewBody = happensAtSDFluent(endI(U),T)
	;
	Timespan = [T1, T2],
	NewBody = (happensAtSDFluent(endI(U),T), T1=<T, T<T2)
	),
	!.
% ------------------------- compile endI ------------------------
*/


% input entity/event
compileConditions1(happensAt(E,T), NewBody, Timespan, _Cyclic) :-
	copy_term(E, E1), inputEntity(E1),
	(
		Timespan = [],
		NewBody = happensAtIE(E,T)
		;
		Timespan = [T1, T2],
		NewBody = (happensAtIE(E,T), T1=<T, T<T2)
	), !.

% output entity/event
compileConditions1(happensAt(E,T), NewBody, Timespan, _Cyclic) :-
	copy_term(E, E1), outputEntity(E1),
	indexOf(Index, E),
	(
		Timespan = [],
		NewBody = happensAtProcessed(Index,E,T)
		;
		Timespan = [T1, T2],
		NewBody = (happensAtProcessed(Index,E,T), T1=<T, T<T2)
	), !.


%%% initiatedAt/2

compileConditions1(initiatedAt(U,T), NewBody, Timespan, _Cyclic) :-
	(
		Timespan = [],
		NewBody = initiatedAt(U,T)
		;
		Timespan = [T1, T2],
		NewBody = initiatedAt(U,T1,T,T2)
	), !.

%%% terminatedAt/2

compileConditions1(terminatedAt(U,T), NewBody, Timespan, _Cyclic) :-
	(
		Timespan = [],
		NewBody = terminatedAt(U,T)
		;
		Timespan = [T1, T2],
		NewBody = terminatedAt(U,T1,T,T2)
	), !.


%%% holdsAt

% simple fluent
compileConditions1(holdsAt(F=V,T), NewBody, _Timespan, Cyclic) :-
	copy_term(F=V, U1), simpleFluent(U1),
	indexOf(Index, F=_),
	(
		Cyclic == true,
		cyclic(U1),
		NewBody = holdsAtCyclic(Index,F=V,T)
		;
		NewBody = holdsAtProcessedSimpleFluent(Index,F=V,T)
	), !.

compileConditions1(holdsAt(I,F=V,T), NewBody, _Timespan, Cyclic) :-
	copy_term(F=V, U1), simpleFluent(U1),
	indexOf(I, F=_),
	(
		Cyclic == true,
		cyclic(U1),
		NewBody = holdsAtCyclic(I,F=V,T)
		;
		NewBody = holdsAtProcessedSimpleFluent(I,F=V,T)
	), !.

% input entity/statically determined fluent
compileConditions1(holdsAt(F=V,T), rtec::holdsAtProcessedIE(Index,F=V,T), _Timespan, _Cyclic) :-
	copy_term(F=V, U1), sDFluent(U1), inputEntity(U1),
	indexOf(Index, F=_), !.

% internal entity/statically determined fluent
compileConditions1(holdsAt(F=V,T), rtec::holdsAtProcessedIE(Index,F=V,T), _Timespan, _Cyclic) :-
	copy_term(F=V, U1), sDFluent(U1), internalEntity(U1),
	indexOf(Index, F=_), !.

% output entity/statically determined fluent
compileConditions1(holdsAt(F=V,T), rtec::holdsAtProcessedSDFluent(Index,F=V,T), _Timespan, _Cyclic) :-
	copy_term(F=V, U1), sDFluent(U1), outputEntity(U1),
	indexOf(Index, F=_), !.

% statically determined fluent that is neither input nor output entity
compileConditions1(holdsAt(F=V,T), rtec::holdsAtSDFluent(F=V,T), _Timespan, _Cyclic) :-
	copy_term(F=V, U1), sDFluent(U1), !.

%%% holdsFor

% simple fluent
compileConditions1(holdsFor(F=V,I), rtec::holdsForProcessedSimpleFluent(Index,F=V,I), _Timespan, _Cyclic) :-
	copy_term(F=V, U1), simpleFluent(U1),
	indexOf(Index, F=_), !.

% input entity/statically determined fluent
compileConditions1(holdsFor(F=V,I), rtec::holdsForProcessedIE(Index,F=V,I), _Timespan, _Cyclic) :-
	copy_term(F=V, U1), sDFluent(U1), inputEntity(U1),
	indexOf(Index, F=_), !.

% internal entity/statically determined fluent
compileConditions1(holdsFor(F=V,I), rtec::holdsForProcessedIE(Index,F=V,I), _Timespan, _Cyclic) :-
	copy_term(F=V, U1), sDFluent(U1), internalEntity(U1),
	indexOf(Index, F=_), !.

% output entity/statically determined fluent
compileConditions1(holdsFor(F=V,I), rtec::holdsForProcessedSDFluent(Index,F=V,I), _Timespan, _Cyclic) :-
	copy_term(F=V, U1), sDFluent(U1), outputEntity(U1),
	indexOf(Index, F=_), !.

% statically determined fluent that is neither input nor output entity
compileConditions1(holdsFor(F=V,I), holdsForSDFluent(F=V,I), _Timespan, _Cyclic) :-
	copy_term(F=V, U1), sDFluent(U1), !.


%%% other body literals, eg interval manipulation constructs
%%% or optimisation checks

% special case for findall
compileConditions1(findall(Targets,user:ECPred,List),findall(Targets,NewECPred,List), Timespan, Cyclic) :-
	compileConditions(ECPred, NewECPred, Timespan, Cyclic), !.

compileConditions1(findall(Targets,ECPred,List),findall(Targets,NewECPred,List), Timespan, Cyclic) :-
	compileConditions(ECPred, NewECPred, Timespan, Cyclic), !.

compileConditions1(Something, Something, _Timespan, _Cyclic).

compileConditions1(Something, _T1, _T2, Something).

:- end_object.
