package unit.issues;

class Issue7540 extends unit.Test {
	function test() {
		eq(1, get());
	}

	function get() {
		return try throw new Error() catch (e:Error) 1;
	}
}

private typedef Error = TypedError<Dynamic>;
private class TypedError<T> {
	public function new() { }
}