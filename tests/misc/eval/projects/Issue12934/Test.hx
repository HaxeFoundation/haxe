class Test {
	static function foo(x:Int, ?v:Float, ?a:Type.ValueType, ?f:Void->Void) {}

	static function main() {
		// `TInt` must skip ?v:Float and resolve as Type.ValueType.TInt against ?a,
		// so the only error is the unresolved `x` inside the function-literal body
		// (reported in place), not a false "Unknown identifier : TInt" against ?v.
		foo(0, TInt, function() x);
	}
}
