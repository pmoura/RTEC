
% handleApplication(+Prolog, +ApplicationName, -LogFile, -WM, -Step, -LastTime, -StreamOrderFlag, -PreprocessingFlag, -ClockTick)

% This is a predicate for setting the appropriate parameters for executing an application (see +ApplicationName),
% and consulting the relevant compiled event description, declarations and dataset. 
% Execution parameters:
% LogFile: the file recording the statistics of execution, WM: working memory size, Step: step size,
% LastTime: the last time-point of the dataset, StreamOrderFlag: 'ordered' or 'unordered' dataset, 
% PreprocessingFlag: 'preprocessing' or 'nopreprocessing', 
% ClockTick: temporal distance between two consecutive time-points, SDEBatch: the input narrative size asserted in a single batch

handleApplication(Prolog, toy, LogFile, ResultFile, WM, Step, LastTime, StreamOrderFlag, PreprocessingFlag, ClockTick, SDEBatch) :- 
	(Prolog=yap, 
	 LogFile = '../examples/toy/experiments/execution log files/log-YAP-toy.txt',
	 ResultFile = '../examples/toy/experiments/execution log files/log-YAP-toy-recognised-intervals.txt'
	 ;
	 Prolog=swi,
	 LogFile = '../examples/toy/experiments/execution log files/log-SWI-toy.txt',
	 ResultFile = '../examples/toy/experiments/execution log files/log-SWI-toy-recognised-intervals.txt'
	),
	WM = 30,
	Step = 30, 
	LastTime = 30,
	StreamOrderFlag = ordered,
	PreprocessingFlag = nopreprocessing, 
	ClockTick = 1,
	SDEBatch = 10,
	consult('../examples/toy/patterns/toy_rules_compiled.prolog'),
	consult('../examples/toy/patterns/toy_declarations.prolog'),
	consult('../examples/toy/experiments/data/toy_data.prolog'),
	consult('../examples/toy/experiments/data/toy_var_domain.prolog'), !.

handleApplication(Prolog, caviar, LogFile, ResultFile, WM, Step, LastTime, StreamOrderFlag, PreprocessingFlag, ClockTick, SDEBatch) :-
	(Prolog=yap,
	 LogFile = '../examples/caviar/experiments/execution log files/log-YAP-caviar-100K-100K.txt',
	 ResultFile = '../examples/caviar/experiments/execution log files/log-YAP-caviar-100K-100K-recognised-intervals.txt'
	 ;
	 Prolog=swi,
	 LogFile = '../examples/caviar/experiments/execution log files/log-SWI-caviar-100K-100K.txt',
	 ResultFile = '../examples/caviar/experiments/execution log files/log-SWI-caviar-100K-100K-recognised-intervals.txt'
	),
	WM = 100000,
	Step = 100000, 
	LastTime = 1007000,
	StreamOrderFlag = ordered,
	PreprocessingFlag = preprocessing, 
	ClockTick = 40,
	SDEBatch = 1000,
	%%%%%%%% LOAD THE APPLICATION-SPECIFIC PRE-PROCESSING MODULE %%%%%%%%
	consult('../examples/caviar/patterns/pre-processing.prolog'),
	consult('../examples/caviar/patterns/caviar_declarations.prolog'),
	consult('../examples/caviar/patterns/compiled_caviar_patterns.prolog'),
	consult('../examples/caviar/experiments/data/updateSDE-caviar.prolog'),
	consult('../examples/caviar/experiments/data/appearance.prolog'),
	consult('../examples/caviar/experiments/data/movementB.prolog'), 
	consult('../examples/caviar/experiments/data/list-of-ids.prolog'), !.

handleApplication(Prolog, ctm, LogFile, ResultFile, WM, Step, LastTime, StreamOrderFlag, PreprocessingFlag, ClockTick, SDEBatch) :-
	(Prolog=yap,
	 LogFile = '../examples/ctm/experiments/execution log files/log-YAP-ctm-10K-10K.txt',
	 ResultFile = '../examples/ctm/experiments/execution log files/log-YAP-ctm-10K-10K-recognised-intervals.txt'
	 ;
	 Prolog=swi,
	 LogFile = '../examples/ctm/experiments/execution log files/log-SWI-ctm-10K-10K.txt',
	 ResultFile = '../examples/ctm/experiments/execution log files/log-SWI-ctm-10K-10K-recognised-intervals.txt'
	),
	WM = 10000,
	Step = 10000, 
	LastTime = 50000,
	StreamOrderFlag = unordered,
	PreprocessingFlag = nopreprocessing, 
	ClockTick = 1,
	SDEBatch = 1000,
	consult('../examples/ctm/patterns/ctm_declarations.prolog'),
	consult('../examples/ctm/patterns/compiled_ctm_patterns.prolog'),
	consult('../examples/ctm/experiments/data/updateSDE-ctm.prolog'),
	consult('../examples/ctm/experiments/data/load-ctm-data.prolog'),
	consult('../examples/ctm/experiments/data/vehicles.prolog'), !.



