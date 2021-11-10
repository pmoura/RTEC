
:- initialization((
	logtalk_load('resources/patterns/caviar_declarations.prolog', [hook(hook_pipeline([directives_hook,compiler(declarations)]))]),
	logtalk_load('resources/patterns/caviar.patterns', [hook(hook_pipeline([directives_hook,compiler(rules)]))]),
	logtalk_load('resources/auxiliary/pre-processing.prolog'),
	logtalk_load('dataset/prolog/appearance.prolog', [hook(directives_hook)]),
	logtalk_load('dataset/prolog/movementB.prolog', [hook(directives_hook)]),
	logtalk_load('dataset/prolog/updateSDE-caviar.prolog', [hook(directives_hook)]),
	rtec::initialiseRecognition(ordered, no, preprocessing, 1)	% ?????
)).
