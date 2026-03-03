package issue7877;

@:build(issue7877.ProcessMacro.build()) class ProcessedClass {
	final foo:Bool;

	function bar() {
		trace(foo);
	}
}
