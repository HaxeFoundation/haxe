# Tests

We have a number of test suites, which are placed in their own folders in this directory.

"RunCi.hx" is the script used by our CIs to run all the test suites. It is possible to run it in local machines too:

 1. Change to this directory.
 2. Install lib used by the script: `haxelib git hx-yaml https://github.com/mikestead/hx-yaml master src`.
 3. Compile the script: `haxe -neko RunCi.n -main RunCi -lib hx-yaml`.
 4. Define the test target by 'export TEST=$TARGET', where "$TARGET" should be one of `macro`, `neko`, `js`, `php`, `cpp`, `flash9`, `as3`, `java`, `cs`, `python`, or `third-party`. However, `flash9`, `as3`, and `third-party` are not likely to work on local machines (TODO).
 5. Run it: `neko RunCi.n`.

Note that the script will try to look for test dependencies and install them if they are not found. Look at the `getXXXDependencies` functions for the details.

## Unit tests

The "unit" folder contains a set of unit tests for the Haxe std library. Unit tests can be run separately instead of using "RunCi.hx", which runs all test suites.

Assuming all test dependencies has been installed, we compile and run the unit tests for all targets at once as follows:

 1. Change to the "unit" directory.
 2. Compile: `haxe compile.hxml`.
 3. Start a dev server: `nekotools server`.
 4. Open `http://localhost:2000/unit.html` in your browser.