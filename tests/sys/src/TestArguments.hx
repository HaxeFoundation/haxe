/**
	This test is intented to be used by TestSys and io.TestProcess.
	It will write the result to "temp/TestArguments.txt" (for debugging).
*/
class TestArguments extends haxe.unit.TestCase {
	static public var expectedArgs(get, null):Array<String>;
	static function get_expectedArgs() {
		return expectedArgs != null ? expectedArgs : expectedArgs = [
			for (arg in new haxe.xml.Fast(Xml.parse(haxe.Resource.getString("args.xml"))).node.args.nodes.arg)
			arg.innerData
		];
	}

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
	#elseif php
		"bin/php/TestArguments/index.php";
	#else
		null;
	#end

	static public var log = "temp/TestArguments.txt";

	function testArgs() {
		var args = Sys.args();
		// trace(args);
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