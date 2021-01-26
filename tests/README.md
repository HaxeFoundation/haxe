# Tests

We have a number of test suites, which are placed in their own folders in this directory.

"RunCi.hx" is the script used by our CIs to run all the test suites.

### Local testing

It is possible to run it in local machines too:

 1. Change to this directory.
 2. Compile the script: `haxe RunCi.hxml`.
 3. Define the test target by `export TEST=$TARGET` (or `set "TEST=$TARGET"` on Windows), where `$TARGET` should be a comma-seperated list of targets, e.g. `neko,macro`. Possible targets are `macro`, `neko`, `js`, `lua`, `php`, `cpp`, `flash9`, `java`, `cs`, `python`, and `third-party`. However, `flash9` and `third-party` are not likely to work on local machines (TODO).
 4. Run it: `neko RunCi.n`.

Note that the script will try to look for test dependencies and install them if they are not found. Look at the `getXXXDependencies` functions for the details.

## Unit tests

The "unit" folder contains a set of unit tests for the Haxe std library. Unit tests can be run separately instead of using "RunCi.hx", which runs all test suites.

Assuming all test dependencies has been installed, we compile and run the unit tests for all targets at once as follows:

 1. Change to the "unit" directory.
 2. Compile: `haxe compile.hxml`.
 3. Start a dev server: `nekotools server`.
 4. Open [http://localhost:2000/unit.html](http://localhost:2000/unit.html) in your browser.

### Cpp unit tests

Cpp unit tests are compiled with `-D HXCPP_NO_DEBUG_LINK` (removes debug symbols) to speed up compilation times. You can remove this from `compile-cpp.hxml` to be able to open the generated `Test-debug.exe` in Visual Studio and debug it. This is useful if it's difficult to figure out why a test is failing, or also _which_ test is failing (for instance with a segmentation fault).

## Sys tests

The "sys" folder contains tests for the system targets. It can also be run separately instead of using "RunCi.hx".

Assuming all test dependencies has been installed, we compile and run the sys tests for all targets at once as follows:

 1. Change to the "sys" directory.
 2. If you're on Windows, comment out the relevant lines in "run.hxml". `haxe run.hxml`.
