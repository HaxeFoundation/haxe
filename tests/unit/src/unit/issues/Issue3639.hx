package unit.issues;

class MyClass3639<T> {
	public function new() { }

	@:generic
	static public function testStatic<K>(k:K) { return k; }

	@:generic
	static public function eachStatic<T, I:Iterator<T>>(itr:I, f:T -> T) {
		for (x in itr) {
			f(x);
		}
	}

	@:generic
	public function testMember<K>(t:T, k:K) { }

	@:generic
	public function eachMember<I:Iterator<T>>(itr : I, f : T -> T) {
		for (x in itr) {
			f(x);
		}
	}
}

class Issue3639 extends Test {
	@:analyzer(no_local_dce)
	function test() {
		MyClass3639.testStatic(1);
		MyClass3639.eachStatic(0...5, function(x) return x);

		hsf(Type.resolveClass('unit.issues.MyClass3639_testStatic_Int'), "testStatic");
		hsf(Type.resolveClass('unit.issues.MyClass3639_eachStatic_Int_IntIterator'), "eachStatic");

		var t = new MyClass3639();
		t.testMember(1, "12");

		var t = new MyClass3639();
		t.eachMember(0...5, function(x) return x);

		hf(MyClass3639, "testMember_String");
		hf(MyClass3639, "eachMember_IntIterator");

		noAssert();
	}
}