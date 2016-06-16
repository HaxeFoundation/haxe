import Issue5351;

class TestCppia
{
	static function main()
	{
		var x:TestCppia = null;
		var runner = new haxe.unit.TestRunner();
		runner.add(new Issue5351());
		trace('loading cppia');
		untyped __global__.__scriptable_load_cppia(sys.io.File.getContent("bin/cppia.cppia"));
		var code = runner.run() ? 0 : 1;
		Sys.exit(code);
	}
}
