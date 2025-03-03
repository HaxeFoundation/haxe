package issue7877;

@:build(issue7877.ProcessMacro.build()) class ProcessedClass {
	final foo:Bool; // = false;

	function bar() {
		trace(foo);
	}
}
