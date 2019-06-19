import cs.system.WeakReference_1;

class Main {
	public static function main() {
		new Test<A>(new A());
	}
}

@:nativeGen
class Test<T:A> {
	public function new(a:T) {
		var ref:WeakReference_1<T> = new WeakReference_1(a);
	}
}

class A {
	public function new() {}
}

