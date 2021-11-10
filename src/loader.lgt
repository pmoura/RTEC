
:- if(current_logtalk_flag(prolog_dialect, swi)).
	:- set_prolog_flag(toplevel_print_options, [max_depth(400)]).
:- endif.

:- initialization((
	logtalk_load(basic_types(loader)),
	logtalk_load(sets(loader)),
	logtalk_load(hook_objects(loader)),
	logtalk_load(hook_flows(loader)),
	set_logtalk_flag(portability, warning),
	logtalk_load('utilities/intervals'),
	logtalk_load(rtec),
	logtalk_load(compiler),
	logtalk_load(directives_hook)
)).

:- multifile([
	% these predicates may be part of the declarations of an event description 
	inputEntity/1, internalEntity/1, outputEntity/1, index/2, event/1,
	simpleFluent/1, sDFluent/1, grounding/1, dgrounded/2,
	% these predicates may be part of an event description 
	holdsFor/2, holdsAt/2, holdsForSDFluent/2, initially/1, initiatedAt/2,
	terminatedAt/2, initiates/3, terminates/3, initiatedAt/4,
	terminatedAt/4, happensAt/2, maxDuration/3, maxDurationUE/3,
	% these predicates may appear in the data files of an application
	updateSDE/2, updateSDE/3, updateSDE/4,
	collectIntervals/1, collectIntervals2/2, buildFromPoints/1,
	buildFromPoints2/2, cyclic/1
]).

/*
:- multifile([
	% holdsFor/2 and happensAt/2 are defined in this file and may also be defined in an event description
	holdsFor/2, happensAt/2,
	% these predicates may appear in the data files of an application
	updateSDE/2, updateSDE/3, updateSDE/4,
	% these predicates are used in processSimpleFluent.prolog
	initially/1, initiatedAt/4, terminatedAt/4
]).

:- multifile([
	% these predicates are defined in this file 
	happensAtProcessedIE/3, happensAtProcessedSDFluent/3, happensAtProcessedSimpleFluent/3, deadlines1/3,
	% these predicates may be part of the declarations of an event description 
	inputEntity/1, internalEntity/1, outputEntity/1, index/2, event/1, simpleFluent/1, sDFluent/1, grounding/1, dgrounded/2,
	% these predicates may be part of an event description 
	holdsFor/2, holdsForSDFluent/2, initially/1, initiatedAt/2, terminatedAt/2, initiates/3, terminates/3, initiatedAt/4, terminatedAt/4, happensAt/2, maxDuration/3, maxDurationUE/3,
	% this predicate may appear in the data files of an application
	updateSDE/4
]).

:- discontiguous([
% these predicates are defined in this file 
happensAtProcessedIE/3, happensAtProcessedSDFluent/3, happensAtProcessedSimpleFluent/3, deadlines1/3,
% these predicates may be part of the declarations of an event description 
inputEntity/1, internalEntity/1, outputEntity/1, index/2, event/1, simpleFluent/1, sDFluent/1, grounding/1, dgrounded/2,
% these predicates may be part of an event description 
holdsFor/2, holdsForSDFluent/2, initially/1, initiatedAt/2, terminatedAt/2, initiates/3, terminates/3, initiatedAt/4, terminatedAt/4, happensAt/2, maxDuration/3, maxDurationUE/3,
% this predicate may appear in the data files of an application
updateSDE/4
]).

:- dynamic([
	temporalDistance/1, input/1, noDynamicGrounding/0, preProcessing/1,
	initTime/1, iePList/4, simpleFPList/4, sdFPList/4, evTList/3,
	happensAtIE/2, holdsForIESI/2, holdsAtIE/2, processedCyclic/2,
	initiallyCyclic/1, storedCyclicPoints/3, startingPoints/3
]).
*/
