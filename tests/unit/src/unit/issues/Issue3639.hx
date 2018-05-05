package unit.issues;

private class MyClass<T> {
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
		MyClass.testStatic(1);
		MyClass.eachStatic(0...5, function(x) return x);

		#if !cs
		// https://github.com/HaxeFoundation/haxe/issues/3658
		hsf(MyClass, "testStatic_Int");
		hsf(MyClass, "eachStatic_Int_IntIterator");
		#end

		var t = new MyClass();
		t.testMember(1, "12");

		var t = new MyClass();
		t.eachMember(0...5, function(x) return x);

		#if !cs
		hf(MyClass, "testMember_String");
		hf(MyClass, "eachMember_IntIterator");
		#end
	}
}