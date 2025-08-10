test:
	@swipl -g "load_files([tests/test_pa]), run_tests" -t halt

docserver:
	@swipl -g "load_files([docs])"

.PHONY: docs
docs:
	swipl -g "load_files(plunit_assert), doc_save(., \
		[format(html), doc_root('./docs'), \
		title('API Documentation for plunit_assert')])" -t halt
