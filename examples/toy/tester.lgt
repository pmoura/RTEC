
:- initialization((
	set_logtalk_flag(report, warnings),
	logtalk_load('../../src/loader'),
	logtalk_load(toy),
	logtalk_load(lgtunit(loader)),
	logtalk_load(tests, [hook(lgtunit)]),
	tests::run
)).
