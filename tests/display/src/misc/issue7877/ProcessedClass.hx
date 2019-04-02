package misc.issue7877;

@:build(misc.issue7877.ProcessMacro.build()) class ProcessedClass {
	final foo:Bool; // = false;

	function bar() {
		trace(foo);
	}
}
