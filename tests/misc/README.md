# Miscellaneous Tests

## Projects

`tests/misc/projects` contains directories with haxe projects, which are executed on CI one by one separately during the execution of a macro test suite.

Each project should contain `compile.hxml` and/or `compile-fail.hxml`. Test suite will look for those files and supply them to Haxe compiler.

If `compile.hxml` was found and `haxe compile.hxml` finished with exit code `0` then a test is considered successful.

If `compile-fail.hxml` was found then `haxe compile-fail.hxml` is executed and stderr of that command is compared to the contents of `compile-fail.hxml.stderr` file located alongside `compile-fail.hxml`. And if stderr content matches `compile-fail.hxml.stderr` then a test is considered successful.

Multiple tests could be executed within a single project directory. Just postfix `hxml` files with a number like these:
- `compile1.hxml`
- `compile2.hxml`
- `compile1-fail.hxml` and `compile1-fail.hxml.stderr`
- `compile2-fail.hxml` and `compile2-fail.hxml.stderr`

### Running `project` tests locally

Chdir to `tests/misc` and run `haxe compile.hxml`.

To run tests only for a single project use the following command: `haxe -D MISC_TEST_FILTER=1234 compile.hxml`, where `1234` are taken from the project directory name `Issue1234`.

### Running target specific projects locally

Tests specific to some targets (python, hl) reside in their own separate folder (respectively `tests/misc/python` and `tests/misc/hl`).

Chdir to `tests/misc/{target}` and run `haxe run.hxml` to run these tests.

To run tests only for a single project use the following command: `haxe -D MISC_TEST_FILTER=1234 run.hxml`, where `1234` are taken from the project directory name `Issue1234`.
