
% handleApplication(+Prolog, +ApplicationName, -InputMode, -LogFile, -WM, -Step, -EndReasoningTime, -StreamOrderFlag, -DynamicGroundingFlag, -PreprocessingFlag, -ForgetThreshold, -DynamicGroundingThreshold, -ClockTick)

% This is a predicate for setting the appropriate parameters for executing an application (see +ApplicationName),
% and consulting the relevant compiled event description, declarations and dataset. 
% Execution parameters:
% InputMode: read input data from CSV files or Prolog files
% LogFile: the file recording the statistics of execution, ResultsFile: the file recording the recognised intervals, WM: working memory size, Step: step size,
% StartReasoningTime and EndReasoningTime specify the range of continuous queries, ie continuous queries take place in (StartReasoningTime, EndReasoningTime]
% the last time-point of the dataset, StreamOrderFlag: 'ordered' or 'unordered' dataset, 
% DynamicGroundingFlag: 'dynamicgrounding' or 'nodynamicgrounding', PreprocessingFlag: 'preprocessing' or 'nopreprocessing', 
% ForgetThreshold: a parameter which helps decide which forget mechanism will be used. The mechanisms differ regarding the usage of the retract/retractall predicates
% 				If the number of events (to be retracted) per time-point is greater than ForgetThreshold, use retractall at each time-point. 
% 				Else, retract every event one by one.
%				Set to '-1' to avoid using retractall.
% DynamicGroundingThreshold: a parameter which helps decide the retract-assert mechanism of dynamic grounding. Again, the cases differ with respect to retract/retractall.
%				If the ratio of the size of the intersection of old ground terms and new ground terms to the number of old ground terms is greater than DynamiGroundingThreshold, do not retract/assert that intersection.
%				Else, retractall ground terms and re-assert new ground terms.
%				Set to '-1' to avoid using retractall.   
% ClockTick: temporal distance between two consecutive time-points, SDEBatch: the input narrative size asserted in a single batch


/*
datasetType/1 is used for logging the recognised intervals in continuousQueries.prolog
datasetType/1 is asserted in this file, and checked in continuousQueries.prolog
datasetType(ground_truth) denotes that the recognitions on the corresponding dataset will be used as ground truth
*/
:- dynamic(datasetType/1).


%%%%%%%%%%%%%%%%%%%%%%%% TOY EXAMPLE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handleApplication(Prolog, toycsv, InputMode, LogFile, ResultsFile, WM, Step, StartReasoningTime, EndReasoningTime, StreamOrderFlag, DynamicGroundingFlag, PreprocessingFlag, ForgetThreshold, DynamicGroundingThreshold, ClockTick, SDEBatch) :- 
	(Prolog=yap, 
	 LogFile = '../examples/toy/results/log-YAP-toy-csv.txt',
	 ResultsFile = '../examples/toy/results/log-YAP-toy-csv-recognised-intervals.txt'
	 ;
	 Prolog=swi,
	 LogFile = '../examples/toy/results/log-SWI-toy-csv.txt',
	 ResultsFile = '../examples/toy/results/log-SWI-toy-csv-recognised-intervals.txt'
	),
	WM = 30,
	Step = 30, 
	StartReasoningTime = 0,
	EndReasoningTime = 30,
	StreamOrderFlag = ordered,
	DynamicGroundingFlag = nodynamicgrounding,
	PreprocessingFlag = nopreprocessing, 
	ForgetThreshold = -1, 
	DynamicGroundingThreshold = -1, 
	ClockTick = 1,
	SDEBatch = 10,
	consult('../examples/toy/resources/patterns/toy_rules_compiled.prolog'),
	consult('../examples/toy/resources/patterns/toy_declarations.prolog'),
	% load the csv file with input data stream	
	InputMode = csv(['../examples/toy/dataset/csv/toy_data.csv']), 
	consult('../examples/toy/dataset/auxiliary/toy_var_domain.prolog'), !.

handleApplication(Prolog, toy, InputMode, LogFile, ResultsFile, WM, Step, StartReasoningTime, EndReasoningTime, StreamOrderFlag, DynamicGroundingFlag, PreprocessingFlag, ForgetThreshold, DynamicGroundingThreshold, ClockTick, SDEBatch) :- 
	(Prolog=yap, 
	 LogFile = '../examples/toy/results/log-YAP-toy.txt',
  	 ResultsFile = '../examples/toy/results/log-YAP-toy-recognised-intervals.txt'
	 ;
	 Prolog=swi,
	 LogFile = '../examples/toy/results/log-SWI-toy.txt',
	 ResultsFile = '../examples/toy/results/log-SWI-toy-recognised-intervals.txt'
	),
	WM = 30,
	Step = 30, 
	StartReasoningTime = 0,
	EndReasoningTime = 30,
	StreamOrderFlag = ordered,
	DynamicGroundingFlag = nodynamicgrounding,
	PreprocessingFlag = nopreprocessing, 
	ForgetThreshold = -1, 
	DynamicGroundingThreshold = -1, 
	ClockTick = 1,
	SDEBatch = 10,
	consult('../examples/toy/resources/patterns/toy_rules_compiled.prolog'),
	consult('../examples/toy/resources/patterns/toy_declarations.prolog'),
	InputMode = dynamic_predicates(['../examples/toy/dataset/prolog/toy_data.prolog']), 
	consult('../examples/toy/dataset/auxiliary/toy_var_domain.prolog'), !.

%%%%%%%%%%%%%%%%%%%%%%%% NETBILL %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Generate an event narrative for Netbill using our synthetic dataset generator and process it with RTEC. Uses dynamic grounding. %%
handleApplication(Prolog, netbill, InputMode, LogFile, ResultsFile, WM, Step, StartReasoningTime, EndReasoningTime, StreamOrderFlag, DynamicGroundingFlag, PreprocessingFlag, ForgetThreshold, DynamicGroundingThreshold, ClockTick, SDEBatch) :-
	WM = 10,
	Step = 10, 
	AgentNo = 1000,
	Seed = 1, 
	add_info('../examples/netbill/results/log-netbill', '.txt', [Prolog, WM, Step, AgentNo, Seed], LogFile),
	add_info('../examples/netbill/results/log-netbill', '-recognised-intervals.txt', [Prolog, WM, Step, AgentNo, Seed], ResultsFile),
	StartReasoningTime = 0,
	EndReasoningTime = 100,
	StreamOrderFlag = unordered,
	DynamicGroundingFlag = dynamicgrounding,
	PreprocessingFlag = nopreprocessing, 
	ForgetThreshold = 10, 
	DynamicGroundingThreshold = 0.8, 
	ClockTick = 1,
	SDEBatch = 10,
	(Prolog=yap, 
	 srandom(Seed) ;
	 Prolog=swi,
	 set_random(seed(Seed))),
	consult('../examples/netbill/resources/patterns/netbill_RTEC_compiled_dg.prolog'),
	consult('../examples/netbill/resources/patterns/netbill_RTEC_declarations_dg.prolog'),
	consult('../examples/netbill/dataset/auxiliary/negotiation-static_generator.prolog'),
	assert_n_agents(AgentNo),
	InputMode = dynamic_predicates(['../examples/netbill/dataset/prolog/negotiation-data_generator.prolog']), !.

%% Run Netbill for an event narrative stored in a csv file. Uses dynamic grounding. %%
handleApplication(Prolog, netbillcsv, InputMode, LogFile, ResultsFile, WM, Step, StartReasoningTime, EndReasoningTime, StreamOrderFlag, DynamicGroundingFlag, PreprocessingFlag, ForgetThreshold, DynamicGroundingThreshold, ClockTick, SDEBatch) :-
	WM = 10,
	Step = 10, 
	AgentNo = 1000,
	Seed = 1, 
	add_info('../examples/netbill/results/log-netbillcsv', '.txt', [Prolog, WM, Step, AgentNo, Seed], LogFile),
	add_info('../examples/netbill/results/log-netbillcsv', '-recognised-intervals.txt', [Prolog, WM, Step, AgentNo, Seed], ResultsFile),
	StartReasoningTime = 0,
	EndReasoningTime = 100,
	StreamOrderFlag = unordered,
	DynamicGroundingFlag = dynamicgrounding,
	PreprocessingFlag = nopreprocessing, 
	ForgetThreshold = 10, 
	DynamicGroundingThreshold = 0.8, 
	ClockTick = 1,
	SDEBatch = 10,
	(Prolog=yap, 
	 srandom(Seed) ;
	 Prolog=swi,
	 set_random(seed(Seed))),
	consult('../examples/netbill/resources/patterns/netbill_RTEC_compiled_dg.prolog'),
	consult('../examples/netbill/resources/patterns/netbill_RTEC_declarations_dg.prolog'),
	consult('../examples/netbill/dataset/auxiliary/negotiation-static_generator.prolog'),
	assert_n_agents(AgentNo),
	add_info('../examples/netbill/dataset/csv/negotiation', '.csv', [AgentNo ,Seed], InputFile),
	InputMode = csv([InputFile]), !.

%%%%%%%%%%%%%%%%%%%%%%%% VOTING %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Generate an event narrative for voting using our synthetic dataset generator and process it with RTEC. Uses dynamic grounding. %%
handleApplication(Prolog, voting, InputMode, LogFile, ResultsFile, WM, Step, StartReasoningTime, EndReasoningTime, StreamOrderFlag, DynamicGroundingFlag, PreprocessingFlag, ForgetThreshold, DynamicGroundingThreshold, ClockTick, SDEBatch) :-
	WM = 10,
	Step = 10, 
	AgentNo = 4000,
	Seed = 1,
	add_info('../examples/voting/results/log-voting', '.txt', [Prolog, WM, Step, AgentNo, Seed], LogFile),
	add_info('../examples/voting/results/log-voting', '-recognised-intervals.txt', [Prolog, WM, Step, AgentNo, Seed], ResultsFile),
	StartReasoningTime = 0,
	EndReasoningTime = 100,
	StreamOrderFlag = unordered,
	DynamicGroundingFlag = dynamicgrounding,
	PreprocessingFlag = nopreprocessing, 
	ForgetThreshold = 10, 
	DynamicGroundingThreshold = 0.8, 
	ClockTick = 1,
	SDEBatch = 10,
	(Prolog=yap, 
	 srandom(Seed) ;
	 Prolog=swi,
	 set_random(seed(Seed))),
	consult('../examples/voting/resources/patterns/vopr_RTEC_compiled_dg.prolog'),
	consult('../examples/voting/resources/patterns/vopr_RTEC_declarations_dg.prolog'),
	consult('../examples/voting/dataset/auxiliary/voting-static_generator.prolog'),
	assert_n_agents(AgentNo),
	InputMode = dynamic_predicates(['../examples/voting/dataset/prolog/voting-data_generator.prolog']), !.

%% Run voting for an event narrative stored in a csv file. Uses dynamic grounding. %%
handleApplication(Prolog, votingcsv, InputMode, LogFile, ResultsFile, WM, Step, StartReasoningTime, EndReasoningTime, StreamOrderFlag, DynamicGroundingFlag, PreprocessingFlag, ForgetThreshold, DynamicGroundingThreshold, ClockTick, SDEBatch) :-
	WM = 10,
	Step = 10, 
	AgentNo = 1000,
	Seed = 1,
	add_info('../examples/voting/results/log-votingcsv', '.txt', [Prolog, WM, Step, AgentNo, Seed], LogFile),
	add_info('../examples/voting/results/log-votingcsv', '-recognised-intervals.txt', [Prolog, WM, Step, AgentNo, Seed], ResultsFile),
	StartReasoningTime = 0,
	EndReasoningTime = 100,
	StreamOrderFlag = unordered,
	DynamicGroundingFlag = dynamicgrounding,
	PreprocessingFlag = nopreprocessing, 
	ForgetThreshold = 10, 
	DynamicGroundingThreshold = 0.8, 
	ClockTick = 1,
	SDEBatch = 10,
	consult('../examples/voting/resources/patterns/vopr_RTEC_compiled_dg.prolog'),
	consult('../examples/voting/resources/patterns/vopr_RTEC_declarations_dg.prolog'),
	consult('../examples/voting/dataset/auxiliary/voting-static_generator.prolog'),
	assert_n_agents(AgentNo),
	add_info('../examples/voting/dataset/csv/voting', '.csv', [AgentNo, Seed], InputFile),
	InputMode = csv([InputFile]), !.


%%%%%%%%%%%%%%%%%%%%%%%% CAVIAR %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handleApplication(Prolog, caviarcsv, InputMode, LogFile, ResultsFile, WM, Step, StartReasoningTime, EndReasoningTime, StreamOrderFlag, DynamicGroundingFlag, PreprocessingFlag, ForgetThreshold, DynamicGroundingThreshold, ClockTick, SDEBatch) :-
	(Prolog=yap,
	 LogFile = '../examples/caviar/results/log-YAP-csv-caviar-100K-100K.txt',
	 ResultsFile = '../examples/caviar/results/log-YAP-csv-caviar-100K-100K-recognised-intervals.txt'
	 ;
	 Prolog=swi,
	 LogFile = '../examples/caviar/results/log-SWI-csv-caviar-100K-100K.txt',
	 ResultsFile = '../examples/caviar/results/log-SWI-csv-caviar-100K-100K-recognised-intervals.txt'
	),
	WM = 100000,
	Step = 100000, 
	StartReasoningTime = 0,
	EndReasoningTime = 1007000,
	StreamOrderFlag = ordered,
	DynamicGroundingFlag = nodynamicgrounding,
	PreprocessingFlag = preprocessing, 
	ForgetThreshold = -1, 
	DynamicGroundingThreshold = -1, 
	ClockTick = 40,
	SDEBatch = 1000,
	%%%%%%%% LOAD THE APPLICATION-SPECIFIC PRE-PROCESSING MODULE %%%%%%%%
	consult('../examples/caviar/resources/auxiliary/pre-processing.prolog'),
	consult('../examples/caviar/resources/patterns/caviar_declarations.prolog'),
	consult('../examples/caviar/resources/patterns/compiled_caviar_patterns.prolog'),
	% load the csv file with input data stream	
	InputMode = csv(['../examples/caviar/dataset/csv/appearance.csv', '../examples/caviar/dataset/csv/movementB.csv']), 
	consult('../examples/caviar/dataset/auxiliary/list-of-ids.prolog'), !.

handleApplication(Prolog, caviar, InputMode, LogFile, ResultsFile, WM, Step, StartReasoningTime, EndReasoningTime, StreamOrderFlag, DynamicGroundingFlag, PreprocessingFlag, ForgetThreshold, DynamicGroundingThreshold, ClockTick, SDEBatch) :-
	(Prolog=yap,
	 LogFile = '../examples/caviar/results/log-YAP-caviar-100K-100K.txt',
	 ResultsFile = '../examples/caviar/results/log-YAP-caviar-100K-100K-recognised-intervals.txt'
	 ;
	 Prolog=swi,
	 LogFile = '../examples/caviar/results/log-SWI-caviar-100K-100K.txt',
	 ResultsFile = '../examples/caviar/results/log-SWI-caviar-100K-100K-recognised-intervals.txt'
	),
	WM = 100000,
	Step = 100000, 
	StartReasoningTime = 0,
	EndReasoningTime = 1007000,
	StreamOrderFlag = ordered,
	DynamicGroundingFlag = nodynamicgrounding,
	PreprocessingFlag = preprocessing, 
	ForgetThreshold = -1, 
	DynamicGroundingThreshold = -1, 
	ClockTick = 40,
	SDEBatch = 1000,
	%%%%%%%% LOAD THE APPLICATION-SPECIFIC PRE-PROCESSING MODULE %%%%%%%%
	consult('../examples/caviar/resources/auxiliary/pre-processing.prolog'),
	consult('../examples/caviar/resources/patterns/caviar_declarations.prolog'),
	consult('../examples/caviar/resources/patterns/compiled_caviar_patterns.prolog'),
	consult('../examples/caviar/dataset/prolog/updateSDE-caviar.prolog'),
	InputMode = dynamic_predicates(['../examples/caviar/dataset/prolog/appearance.prolog', '../examples/caviar/dataset/prolog/movementB.prolog']), 
	consult('../examples/caviar/dataset/auxiliary/list-of-ids.prolog'), !.


%%%%%%%%%%%%%%%%%%%%%%%% CTM %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handleApplication(Prolog, ctmcsv, InputMode, LogFile, ResultsFile, WM, Step, StartReasoningTime, EndReasoningTime, StreamOrderFlag, DynamicGroundingFlag, PreprocessingFlag, ForgetThreshold, DynamicGroundingThreshold, ClockTick, SDEBatch) :-
	(Prolog=yap,
	 LogFile = '../examples/ctm/results/log-YAP-csv-ctm-10K-10K.txt',
	 ResultsFile = '../examples/ctm/results/log-YAP-csv-ctm-10K-10K-recognised-intervals.txt'
	 ;
	 Prolog=swi,
	 LogFile = '../examples/ctm/results/log-SWI-csv-ctm-10K-10K.txt',
	 ResultsFile = '../examples/ctm/results/log-SWI-csv-ctm-10K-10K-recognised-intervals.txt'
	),
	WM = 10000,
	Step = 10000, 
	StartReasoningTime = 0,
	EndReasoningTime = 50000,
	StreamOrderFlag = unordered,
	DynamicGroundingFlag = nodynamicgrounding,
	PreprocessingFlag = nopreprocessing, 
	ForgetThreshold = -1, 
	DynamicGroundingThreshold = -1, 
	ClockTick = 1,
	SDEBatch = 1000,
	consult('../examples/ctm/resources/patterns/ctm_declarations.prolog'),
	consult('../examples/ctm/resources/patterns/compiled_ctm_patterns.prolog'),
	InputMode = csv(['../examples/ctm/dataset/csv/abrupt_acceleration.csv', '../examples/ctm/dataset/csv/abrupt_deceleration.csv', '../examples/ctm/dataset/csv/internal_temperature_change.csv', '../examples/ctm/dataset/csv/noise_level_change.csv', '../examples/ctm/dataset/csv/passenger_density_change.csv', '../examples/ctm/dataset/csv/sharp_turn.csv', '../examples/ctm/dataset/csv/stop_enter_leave.csv']),
	consult('../examples/ctm/dataset/auxiliary/vehicles.prolog'), !.

handleApplication(Prolog, ctm, InputMode, LogFile, ResultsFile, WM, Step, StartReasoningTime, EndReasoningTime, StreamOrderFlag, DynamicGroundingFlag, PreprocessingFlag, ForgetThreshold, DynamicGroundingThreshold, ClockTick, SDEBatch) :-
	(Prolog=yap,
	 LogFile = '../examples/ctm/results/log-YAP-ctm-10K-10K.txt',
	 ResultsFile = '../examples/ctm/results/log-YAP-ctm-10K-10K-recognised-intervals.txt'
	 ;
	 Prolog=swi,
	 LogFile = '../examples/ctm/results/log-SWI-ctm-10K-10K.txt',
	 ResultsFile = '../examples/ctm/results/log-SWI-ctm-10K-10K-recognised-intervals.txt'
	),
	WM = 10000,
	Step = 10000, 
	StartReasoningTime = 0,
	EndReasoningTime = 50000,
	StreamOrderFlag = unordered,
	DynamicGroundingFlag = nodynamicgrounding,
	PreprocessingFlag = nopreprocessing, 
	ForgetThreshold = -1, 
	DynamicGroundingThreshold = -1, 
	ClockTick = 1,
	SDEBatch = 1000,
	consult('../examples/ctm/resources/patterns/ctm_declarations.prolog'),
	consult('../examples/ctm/resources/patterns/compiled_ctm_patterns.prolog'),
	consult('../examples/ctm/dataset/prolog/updateSDE-ctm.prolog'),
	consult('../examples/ctm/dataset/prolog/load-ctm-data.prolog'),
	%InputMode = dynamic_predicates(['../examples/ctm/resources/abrupt_acceleration.prolog', '../examples/ctm/resources/abrupt_deceleration.prolog', '../examples/ctm/resources/internal_temperature_change.prolog', '../examples/ctm/resources/noise_level_change.prolog', '../examples/ctm/resources/passenger_density_change.prolog', '../examples/ctm/resources/sharp_turn.prolog', '../examples/ctm/resources/stop_enter_leave.prolog']),
	consult('../examples/ctm/dataset/auxiliary/vehicles.prolog'), !.


%%%%%%%%%%%%%%%%%%%%%%%% Maritime-Brest %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Use SWI on the maritime datasets only for debugging

%%%%% critical points

handleApplication(Prolog, brest-critical, InputMode, LogFile, ResultsFile, WM, Step, StartReasoningTime, EndReasoningTime, StreamOrderFlag, DynamicGroundingFlag, PreprocessingFlag, ForgetThreshold, DynamicGroundingThreshold, ClockTick, _SDEBatch) :-
	(Prolog=yap,
 	 LogFile = '../examples/maritime/results/log-Brest-critical-yap-1day-1day.txt',
	 ResultsFile = '../examples/maritime/results/log-Brest-critical-yap-1day-1day-recognised-intervals-critical-yap.txt'
	 ;
	 Prolog=swi,
	 LogFile = '../examples/maritime/results/log-Brest-critical-SWI-1day-1day.txt',
	 ResultsFile = '../examples/maritime/results/log-Brest-critical-SWI-1day-1day-recognised-intervals-critical-SWI.txt'
	),
	WM = 86400,
	Step = 86400, 
	% start of the dataset:
	StartReasoningTime = 1443650400,
	EndReasoningTime = 1448834400,
	% end of dataset:
	% EndReasoningTime = 1459548000,
	StreamOrderFlag = unordered,
	DynamicGroundingFlag = dynamicgrounding,
	PreprocessingFlag = nopreprocessing, 
	ForgetThreshold = -1, 
	DynamicGroundingThreshold = -1, 
	ClockTick = 1,
	% load the patterns:
	consult('../examples/maritime/resources/patterns/Maritime_Patterns_Compiled.prolog'),
	% these are auxiliary predicates used in the maritime patterns
	consult('../examples/maritime/resources/auxiliary/compare.prolog'),	
	consult('../examples/maritime/resources/patterns/Maritime_Patterns_Declarations.prolog'),
	% load the dynamic data:
	InputMode = csv(['../examples/maritime/dataset/csv/preprocessed_dataset_RTEC_critical_nd.csv']),
	% load the static data
	consult('../examples/maritime/resources/auxiliary/loadStaticData.prolog'), !.


%%%%% enriched points

handleApplication(Prolog, brest-enriched, InputMode, LogFile, ResultsFile, WM, Step, StartReasoningTime, EndReasoningTime, StreamOrderFlag, DynamicGroundingFlag, PreprocessingFlag, ForgetThreshold, DynamicGroundingThreshold, ClockTick, _SDEBatch) :-
	(Prolog=yap,
 	 LogFile = '../examples/maritime/results/log-Brest-enriched-yap-1day-1day.txt',
	 ResultsFile = '../examples/maritime/results/log-Brest-enriched-yap-1day-1day-recognised-intervals-critical-yap.txt'
	 ;
	 Prolog=swi,
	 LogFile = '../examples/maritime/results/log-Brest-enriched-SWI-1day-1day.txt',
	 ResultsFile = '../examples/maritime/results/log-Brest-enriched-SWI-1day-1day-recognised-intervals-critical-SWI.txt'

	),
	WM = 86400,
	Step = 86400, 
	% start of the dataset:
	StartReasoningTime = 1443650400,
	% end of the first week:
	EndReasoningTime = 1444255200,
	% end of dataset:
	% EndReasoningTime = 1459548000,
	StreamOrderFlag = unordered,
	DynamicGroundingFlag = dynamicgrounding,
	PreprocessingFlag = nopreprocessing, 
	ForgetThreshold = -1, 
	DynamicGroundingThreshold = -1, 
	ClockTick = 1,
	% load the patterns:
	consult('../examples/maritime/resources/patterns/Maritime_Patterns_Compiled.prolog'),
	% these are auxiliary predicates used in the maritime patterns
	consult('../examples/maritime/resources/auxiliary/compare.prolog'),	
	consult('../examples/maritime/resources/patterns/Maritime_Patterns_Declarations.prolog'),
	% load the dynamic data:
	InputMode = csv(['../examples/maritime/dataset/csv/preprocessed_dataset_RTEC_enriched_nd.csv']),
	%%% Important: instruct the execution script that the recognitions on this dataset will be treated as the ground truth
	assertz( datasetType(ground_truth) ),
	% load the static data
	consult('../examples/maritime/resources/auxiliary/loadStaticData.prolog'), !.


%%%% Execution scripts for CLIs 

%test for cli
handleApplication(Prolog, netbillCLI, InputMode, LogFile, ResultsFile, WM, Step, AgentNo, StreamOrderFlag, PreprocessingFlag, ForgetThreshold, DynamicGroundingThreshold, ClockTick, SDEBatch, InputFiles, ResultsPath) :-
	%AgentNo = 1000,
	%Seed = 1,
	atom_concat(ResultsPath, '/log-netbillCLI', LogsPath),
	add_info(LogsPath, '.txt', [Prolog, WM, Step], LogFile),
	add_info(LogsPath, '-recognised-intervals.txt', [Prolog, WM, Step], ResultsFile),
	%StartReasoningTime = 0,
	%EndReasoningTime = 100,
	StreamOrderFlag = unordered,
	PreprocessingFlag = nopreprocessing, 
	ForgetThreshold = 10, 
	DynamicGroundingThreshold = 0.8, 
	ClockTick = 1,
	SDEBatch = 10,
	assert_n_agents(AgentNo),
	InputMode = csv(InputFiles), !.

%test for cli
handleApplication(Prolog, votingCLI, InputMode, LogFile, ResultsFile, WM, Step, AgentNo, StreamOrderFlag, PreprocessingFlag, ForgetThreshold, DynamicGroundingThreshold, ClockTick, SDEBatch, InputFiles, ResultsPath) :-
	%AgentNo = 1000,
	%Seed = 1,
	atom_concat(ResultsPath, '/log-votingCLI', LogsPath),
	add_info(LogsPath, '.txt', [Prolog, WM, Step], LogFile),
	add_info(LogsPath, '-recognised-intervals.txt', [Prolog, WM, Step], ResultsFile),
	%StartReasoningTime = 0,
	%EndReasoningTime = 100,
	StreamOrderFlag = unordered,
	PreprocessingFlag = nopreprocessing, 
	ForgetThreshold = 10, 
	DynamicGroundingThreshold = 0.8, 
	ClockTick = 1,
	SDEBatch = 10,
	assert_n_agents(AgentNo),
	InputMode = csv(InputFiles), !.

handleApplication(Prolog, maritimeCLI, InputMode, LogFile, ResultsFile, WM, Step, _AgentNo, StreamOrderFlag, PreprocessingFlag, ForgetThreshold, DynamicGroundingThreshold, ClockTick, _SDEBatch, InputFiles, ResultsPath) :-
	atom_concat(ResultsPath, '/log-maritimeCLI', LogsPath),
	add_info(LogsPath, '.txt', [Prolog, WM, Step], LogFile),
	add_info(LogsPath, '-recognised-intervals.txt', [Prolog, WM, Step], ResultsFile),
	% start of the dataset:
	%StartReasoningTime = 1443650400,
	%EndReasoningTime = 1448834400,
	% end of dataset:
	% EndReasoningTime = 1459548000,
	StreamOrderFlag = unordered,
	%DynamicGroundingFlag = dynamicgrounding,
	PreprocessingFlag = nopreprocessing, 
	ForgetThreshold = -1, 
	DynamicGroundingThreshold = -1, 
	ClockTick = 1,
	% load the patterns:
	%consult('../examples/Maritime-Monitoring/maritime resources/Maritime_Patterns_Compiled.prolog'),
	% these are auxiliary predicates used in the maritime patterns
	%consult('../examples/Maritime-Monitoring/maritime resources/compare.prolog'),	
	%consult('../examples/Maritime-Monitoring/maritime resources/Maritime_Patterns_Declarations.prolog'),
	% load the dynamic data:
	InputMode = csv(InputFiles), !.
	% load the static data
	%consult('../examples/Maritime-Monitoring/Brest/experiments/data/static/loadStaticData.prolog'), !.

handleApplication(Prolog, caviarCLI, InputMode, LogFile, ResultsFile, WM, Step, _AgentNo, StreamOrderFlag, PreprocessingFlag, ForgetThreshold, DynamicGroundingThreshold, ClockTick, SDEBatch, InputFiles, ResultsPath) :-
	atom_concat(ResultsPath, '/log-caviarCLI', LogsPath),
	add_info(LogsPath, '.txt', [Prolog, WM, Step], LogFile),
	add_info(LogsPath, '-recognised-intervals.txt', [Prolog, WM, Step], ResultsFile),	
	%WM = 100000,
	%Step = 100000, 
	%StartReasoningTime = 0,
	%EndReasoningTime = 1007000,
	StreamOrderFlag = ordered,
	%DynamicGroundingFlag = nodynamicgrounding,
	PreprocessingFlag = preprocessing, 
	ForgetThreshold = -1, 
	DynamicGroundingThreshold = -1, 
	ClockTick = 40,
	SDEBatch = 1000,
	%%%%%%%% LOAD THE APPLICATION-SPECIFIC PRE-PROCESSING MODULE %%%%%%%%
	%consult('../examples/caviar/resources/pre-processing.prolog'),
	%consult('../examples/caviar/resources/caviar_declarations.prolog'),
	%consult('../examples/caviar/resources/compiled_caviar_patterns.prolog'),
	% load the csv file with input data stream	
	%atom_concat(ResultsPath, '/examples/caviar/experiments/data/csv/appearance.csv', AppearanceFile),
	%atom_concat(ResultsPath, '/examples/caviar/experiments/data/csv/movementB.csv', MovementBFile),
	%InputMode = csv([AppearanceFile, MovementBFile]), !.
	InputMode = csv(InputFiles), !.
	%consult('../examples/caviar/experiments/data/list-of-ids.prolog'), !.

handleApplication(Prolog, ctmCLI, InputMode, LogFile, ResultsFile, WM, Step, _AgentNo, StreamOrderFlag, PreprocessingFlag, ForgetThreshold, DynamicGroundingThreshold, ClockTick, SDEBatch, InputFiles, ResultsPath) :-
	atom_concat(ResultsPath, '/log-ctmCLI', LogsPath),
	add_info(LogsPath, '.txt', [Prolog, WM, Step], LogFile),
	add_info(LogsPath, '-recognised-intervals.txt', [Prolog, WM, Step], ResultsFile),	
	%WM = 10000,
	%Step = 10000, 
	%StartReasoningTime = 0,
	%EndReasoningTime = 50000,
	StreamOrderFlag = unordered,
	%DynamicGroundingFlag = nodynamicgrounding,
	PreprocessingFlag = nopreprocessing, 
	ForgetThreshold = -1, 
	DynamicGroundingThreshold = -1, 
	ClockTick = 1,
	SDEBatch = 1000,
	%consult('../examples/ctm/resources/ctm_declarations.prolog'),
	%consult('../examples/ctm/resources/compiled_ctm_patterns.prolog'),
	%InputMode = csv(['../examples/ctm/experiments/data/csv/abrupt_acceleration.csv', '../examples/ctm/experiments/data/csv/abrupt_deceleration.csv', '../examples/ctm/experiments/data/csv/internal_temperature_change.csv', '../examples/ctm/experiments/data/csv/noise_level_change.csv', '../examples/ctm/experiments/data/csv/passenger_density_change.csv', '../examples/ctm/experiments/data/csv/sharp_turn.csv', '../examples/ctm/experiments/data/csv/stop_enter_leave.csv']),
	InputMode = csv(InputFiles), !.
	%consult('../examples/ctm/experiments/data/vehicles.prolog'), !.

handleApplication(Prolog, toyCLI, InputMode, LogFile, ResultsFile, WM, Step, _AgentNo, StreamOrderFlag, PreprocessingFlag, ForgetThreshold, DynamicGroundingThreshold, ClockTick, SDEBatch, InputFiles, ResultsPath) :- 
	atom_concat(ResultsPath, '/log-toyCLI', LogsPath),
	add_info(LogsPath, '.txt', [Prolog, WM, Step], LogFile),
	add_info(LogsPath, '-recognised-intervals.txt', [Prolog, WM, Step], ResultsFile),	
	%WM = 30,
	%Step = 30, 
	%StartReasoningTime = 0,
	%EndReasoningTime = 30,
	StreamOrderFlag = ordered,
	%DynamicGroundingFlag = nodynamicgrounding,
	PreprocessingFlag = nopreprocessing, 
	ForgetThreshold = -1, 
	DynamicGroundingThreshold = -1, 
	ClockTick = 1,
	SDEBatch = 10,
	%consult('../examples/toy/resources/toy_rules_compiled.prolog'),
	%consult('../examples/toy/resources/toy_declarations.prolog'),
	% load the csv file with input data stream	
	InputMode = csv(InputFiles), !. 
	%consult('../examples/toy/resources/toy_var_domain.prolog'), !.

%% Auxiliary predicates to differentiate input/result files based on the parameter of the experiment. %%
%% Usage:   add_info(+PrefixStr, +SuffixStr, +ParametersList, -FinalStr)							  %%
%% Example: add_info('voting-result-file', '.txt', [yap,10,10], 'voting-result-file-yap-10-10.txt')   %%

add_info(PrefixStr, SuffixStr, [], FinalStr):-
	atom_concat(PrefixStr, SuffixStr, FinalStr).

add_info(PrefixStr, SuffixStr, [NewInfo|RestInfo], FinalStr):-
	\+number(NewInfo), !,
	atom_concat(PrefixStr, '-', PrefixStr1),
	atom_concat(PrefixStr1, NewInfo, PrefixStr2),
	add_info(PrefixStr2, SuffixStr, RestInfo, FinalStr).

add_info(PrefixStr, SuffixStr, [NewInfo|RestInfo], FinalStr):-
	current_prolog_flag(dialect, yap), !,
    number_atom(NewInfo, NewInfoAtom),
	atom_concat(PrefixStr, '-', PrefixStr1),
	atom_concat(PrefixStr1, NewInfoAtom, PrefixStr2),
	add_info(PrefixStr2, SuffixStr, RestInfo, FinalStr).

add_info(PrefixStr, SuffixStr, [NewInfo|RestInfo], FinalStr):-
    atom_number(NewInfoAtom, NewInfo), % opposite from above.
	atom_concat(PrefixStr, '-', PrefixStr1),
	atom_concat(PrefixStr1, NewInfoAtom, PrefixStr2),
	add_info(PrefixStr2, SuffixStr, RestInfo, FinalStr).
