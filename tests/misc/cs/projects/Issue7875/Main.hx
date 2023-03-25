import cs.system.WeakReference_1;

class Main {
	public static function main() {
		new Test<A>(new A());
	}
}

@:nativeGen
class Test<T:A> {
	public function new(a:T) {
		test(function() return new WeakReference_1(a));
	}

	function test(cb:()->WeakReference_1<T>):Void {}


}

class A {
	public function new() {}
}

