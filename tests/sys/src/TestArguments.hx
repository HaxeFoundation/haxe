import utest.Assert;
import utest.Runner;
import utest.ui.Report;

/**
	This test is intented to be used by TestSys and io.TestProcess.
*/
class TestArguments extends utest.Test {
	// We may compare and update the test cases of other popular langs/libs: https://gist.github.com/andyli/d55ae9ea1327bbbf749d
	static public var expectedArgs(default, never):Array<String> = [
		"foo",
		"12",

		// symbols
		"&",
		"&&",
		"|",
		"||",
		".",
		",",
		"<",
		">",
		"<<",
		">>",
		":",
		";",
		"(",
		")",
		"( )",

		// backslashes
		"\\",
		"\\\\",
		"\\\\\\",

		// single quote
		"'",
		// kind of an escaped single quote
		"\\'",

		// double quote
		'"',
		// kind of an escaped double quote
		'\\"',

		// space
		" ",
		// kind of an escaped space
		"\\ ",

		// empty string
		"",

		// complex stuff
		"a b  %PATH% $HOME c\\&<>[\\\"]#{}|%$\\\"\"",
	].concat(switch (Sys.systemName()) {
		case "Windows":
		[];
		case _:
		[
			// linebreak
			"\n",

			// Chinese, Japanese
			"中文，にほんご",
		];
	});

	static public var bin:String =
	#if interp
		"TestArguments.hx";
	#elseif neko
		"bin/neko/TestArguments.n";
	#elseif hl
		"bin/hl/TestArguments.hl";
	#elseif cpp
		#if debug
			"bin/cpp/TestArguments-debug";
		#else
			"bin/cpp/TestArguments";
		#end
	#elseif cs
		#if debug
			"bin/cs/bin/TestArguments-Debug.exe";
		#else
			"bin/cs/bin/TestArguments.exe";
		#end
	#elseif jvm
		"bin/jvm/TestArguments.jar";
	#elseif java
		#if debug
			"bin/java/TestArguments-Debug.jar";
		#else
			"bin/java/TestArguments.jar";
		#end
	#elseif python
		"bin/python/TestArguments.py";
	#elseif php
		"bin/php/TestArguments/index.php";
	#elseif lua
		"bin/lua/TestArguments.lua";
	#else
		null;
	#end

	function testArgs() {
		var args = Sys.args();
		for (i in 0...expectedArgs.length) {
			Assert.equals(expectedArgs[i], args[i]);
		}
		Assert.equals(expectedArgs.length, args.length);
	}

	static function main():Void {
		var runner = new Runner();
		var report = Report.create(runner);
		report.displayHeader = AlwaysShowHeader;
		report.displaySuccessResults = NeverShowSuccessResults;
		runner.addCase(new TestArguments());
		runner.run();
	}
}
