
:- initialization((
	logtalk_load(basic_types(loader)),
	logtalk_load(reader(loader)),
	logtalk_load(data_loader, [portability(warning)])
)).
