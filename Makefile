.PHONY: test
test:
	@swipl -g "load_files([tests/test_pa]), run_tests" -t halt

.PHONY: docserver
docserver:
	@swipl -g "load_files([docs])"

.PHONY: docs
docs:
	swipl -g "load_files(plunit_assert), doc_save(., \
		[format(html), doc_root('./docs'), \
		title('API Documentation for plunit_assert')])" -t halt

.PHONY: pack
pack: clean
	# remember to bump version number
	mkdir -p pack/prolog
	cp plunit_assert.pl pack/prolog/
	cp pack.pl.dist pack/pack.pl
	tar -zcvf pack.tgz pack

.PHONY: clean
clean:
	rm -rf pack
