/**
	This test is intented to be used by TestSys and io.TestProcess.
	It will write the result to "temp/TestArguments.txt" (for debugging).
*/
class TestArguments extends haxe.unit.TestCase {
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
	#if neko
		"bin/neko/TestArguments.n";
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
	#elseif java
		#if debug
			"bin/java/TestArguments-Debug.jar";
		#else
			"bin/java/TestArguments.jar";
		#end
	#elseif python
		"bin/python/TestArguments.py";
	#elseif php7
		"bin/php7/TestArguments/index.php";
	#elseif php
		"bin/php/TestArguments/index.php";
	#elseif lua
		"bin/lua/TestArguments.lua";
	#else
		null;
	#end

	static public var log = "temp/TestArguments.txt";

	function testArgs() {
		var args = Sys.args();
		for (i in 0...expectedArgs.length) {
			assertEquals(expectedArgs[i], args[i]);
		}
		assertEquals(expectedArgs.length, args.length);
	}

	static function main():Void {
		var log = sys.io.File.write(log);
		log.writeString(haxe.Json.stringify(Sys.args()) + "\n");
		haxe.unit.TestRunner.print = function(v){
			log.writeString(v);
		};
		var runner = new haxe.unit.TestRunner();
		runner.add(new TestArguments());
		var code = runner.run() ? 0 : 1;
		log.flush();
		log.close();
		Sys.exit(code);
	}
}
