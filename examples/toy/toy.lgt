
:- initialization((
	logtalk_load('resources/patterns/toy_declarations.prolog', [hook(hook_pipeline([directives_hook,compiler(declarations)]))]),
	logtalk_load('resources/patterns/toy_rules.patterns', [hook(hook_pipeline([directives_hook,compiler(rules)]))]),
	logtalk_load('dataset/auxiliary/toy_var_domain.prolog'),
	logtalk_load('dataset/prolog/toy_data.prolog', [hook(directives_hook)]),
	rtec::initialiseRecognition(ordered, no, nopreprocessing, 1),
	updateSDE(0, 10),
	updateSDE(10, 20),
	updateSDE(20, 30),
	rtec::eventRecognition(21, 21)
)).
