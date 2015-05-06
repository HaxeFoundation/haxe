# Tests

We have a number of test suites, which are placed in their own folders in this directory.

"RunCi.hx" is the script used by our CIs to run all the test suites. It is possible to configure CIs for your own fork of Haxe on Github using the instructions as follows.

### TravisCI

TravisCI provides Linux and Mac builds. However, for forks, it only provides Linux builds unless [requested manually](http://docs.travis-ci.com/user/multi-os/).

To set up TravisCI:

 1. Head to TravisCI, go to the [profile page](https://travis-ci.org/profile).
 2. Turn on the switch of your fork. If you couldn't find the repo, try the *Sync* button on the top to refresh the list.
 3. If you want to enable browser testing for the JS target, follow the instructions in the SauceLabs section. If not, go to the next step.
 4. Push to the repo to trigger a new build. The build result should be available at `https://travis-ci.org/<user_name>/haxe`.

### AppVeyor

AppVeyor provides Windows builds.

To set up AppVeyor:

 1. Head to AppVeyor, [add a new project](https://ci.appveyor.com/projects/new).
 2. Select the forked repo under your account.
 3. Push to the repo to trigger a new build. The build result should be available at `https://ci.appveyor.com/project/<user_name>/haxe`.

### SauceLabs

SauceLabs provides browser testings. We use TravisCI to drive the test, so you should have TravisCI configured.

To set up SauceLabs:

 1. Head to the project page of your fork at `https://travis-ci.org/<user_name>/haxe`
 2. Select *Settings* -> *Settings*.
 3. Select the *Environment Variables* tab.
 4. Select *Add a new variable* for the following pairs, keeping the switch off for *Display value in build logs*.
    * name: `SAUCE_USERNAME`, value: your SauceLabs account name
    * name: `SAUCE_ACCESS_KEY`, value: your SauceLabs access key, which can be found at https://saucelabs.com/account
 5. Push to the repo to trigger a new TravisCI build. SauceLabs test results should be available in the JS build log.

### Local testing

It is possible to run it in local machines too:

 1. Change to this directory.
 2. Compile the script: `haxe RunCi.hxml`.
 3. Define the test target by `export TEST=$TARGET` (or `set "TEST=$TARGET"` on Windows), where `$TARGET` should be a comma-seperated list of targets, e.g. `neko,macro`. Possible targets are `macro`, `neko`, `js`, `php`, `cpp`, `flash9`, `as3`, `java`, `cs`, `python`, and `third-party`. However, `flash9`, `as3`, and `third-party` are not likely to work on local machines (TODO).
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
