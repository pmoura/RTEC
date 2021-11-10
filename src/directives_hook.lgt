
:- object(directives_hook,
	implements(expanding)).

	term_expansion(
		begin_of_file,
		[	begin_of_file,
			(:- multifile([
				% these predicates may be part of the declarations of an event description 
				inputEntity/1, internalEntity/1, outputEntity/1, index/2, event/1,
				simpleFluent/1, sDFluent/1, grounding/1, dgrounded/2,
				% these predicates may be part of an event description 
				holdsFor/2, holdsForSDFluent/2, initially/1, initiatedAt/2,
				terminatedAt/2, initiates/3, terminates/3, initiatedAt/4,
				terminatedAt/4, happensAt/2, maxDuration/3, maxDurationUE/3,
				% these predicates may appear in the data files of an application
				updateSDE/2, updateSDE/3, updateSDE/4,
				collectIntervals/1, collectIntervals2/2, buildFromPoints/1,
				buildFromPoints2/2, cyclic/1
			])),
			(:- discontiguous([
				% these predicates may be part of the declarations of an event description 
				inputEntity/1, internalEntity/1, outputEntity/1, index/2, event/1,
				simpleFluent/1, sDFluent/1, grounding/1, dgrounded/2,
				% these predicates may be part of an event description 
				holdsFor/2, holdsForSDFluent/2, initially/1, initiatedAt/2,
				terminatedAt/2, initiates/3, terminates/3, initiatedAt/4,
				terminatedAt/4, happensAt/2, maxDuration/3, maxDurationUE/3,
				% these predicates may appear in the data files of an application
				updateSDE/2, updateSDE/3, updateSDE/4,
				collectIntervals/1, collectIntervals2/2, buildFromPoints/1,
				buildFromPoints2/2, cyclic/1
			]))
		]
	).

:- end_object.
