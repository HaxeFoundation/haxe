# Tests

We have a number of test suites, which are placed in their own folders in this directory.

"RunCi.hx" is the script used by our CIs to run all the test suites. It is possible to run it in local machines too:

 1. Change to this directory.
 2. Compile the script: `haxe RunCi.hxml`.
 3. Define the test target by `export TEST=$TARGET` (or `set "TEST=$TARGET"` on Windows), where `$TARGET` should be one of `macro`, `neko`, `js`, `php`, `cpp`, `flash9`, `as3`, `java`, `cs`, `python`, or `third-party`. However, `flash9`, `as3`, and `third-party` are not likely to work on local machines (TODO).
 4. Run it: `neko RunCi.n`.

Note that the script will try to look for test dependencies and install them if they are not found. Look at the `getXXXDependencies` functions for the details.

## Unit tests

The "unit" folder contains a set of unit tests for the Haxe std library. Unit tests can be run separately instead of using "RunCi.hx", which runs all test suites.

Assuming all test dependencies has been installed, we compile and run the unit tests for all targets at once as follows:

 1. Change to the "unit" directory.
 2. Compile: `haxe compile.hxml`.
 3. Start a dev server: `nekotools server`.
 4. Open `http://localhost:2000/unit.html` in your browser.

## Sys tests

The "sys" folder contains tests for the system targets. It can also be run separately instead of using "RunCi.hx".

Assuming all test dependencies has been installed, we compile and run the sys tests for all targets at once as follows:

 1. Change to the "sys" directory.
 2. If you're on Windows, comment out the relevant lines in "run.hxml". `haxe run.hxml`.