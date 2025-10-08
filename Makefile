.PHONY: test
test:
	@swipl -g "load_files([tests/test_pa]), run_tests" -t halt

.PHONY: testc
testc:
	@swipl -g "load_files([tests/test_pa]), \
		coverage(run_tests, [files([prolog/plunit_assert.pl])])" -t halt

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
	mkdir -p plunit_assert/prolog
	cp plunit_assert.pl plunit_assert/prolog/
	cp pack.pl.dist plunit_assert/pack.pl
	COPYFILE_DISABLE=1 tar --format=ustar --exclude='._*' -czf plunit_assert-0.2.1.tgz plunit_assert

.PHONY: clean
clean:
	rm -f plunit_assert*.tgz
	rm -rf plunit_assert/
