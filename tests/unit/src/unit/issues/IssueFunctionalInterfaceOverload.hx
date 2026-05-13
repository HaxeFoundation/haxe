package unit.issues;

#if jvm
@:functionalInterface
private interface IntOp {
	function apply(a:Int, b:Int):Int;
}

@:functionalInterface
private interface StrOp {
	function apply(a:String):String;
}

private class Overloaded {
	public function new() {}

	@:overload public function call(op:IntOp, a:Int, b:Int):Int return op.apply(a, b);
	@:overload public function call(op:StrOp, s:String):String return op.apply(s);

	@:overload public function single(op:IntOp):Int return op.apply(3, 4);
}
#end

class IssueFunctionalInterfaceOverload extends Test {
	#if jvm
	// Lambda passed where a SAM-parametered overload is expected: the overload
	// resolver used to discard SAM candidates because strict unify rejects
	// TFun-vs-TInst. With FI-aware candidate filtering, the right overload is
	// picked and the cast happens during arg processing.
	function testSamOverloadDisambiguation() {
		var o = new Overloaded();
		eq(7, o.call((a, b) -> a + b, 3, 4));
		eq("HI!", o.call(s -> s + "!", "HI"));
	}

	// Non-overloaded SAM call site — sanity check that the simple path still
	// works (was already supported, kept here so the overload test sits next
	// to a baseline that bisects nicely if either path regresses).
	function testSamSingleOverload() {
		var o = new Overloaded();
		eq(7, o.single((a, b) -> a + b));
	}
	#end
}
