test:
	@swipl -g "load_files([tests/test_pa]), run_tests" -t halt
