@:nativeGen
class C<T> {}

class C2<T> {}

abstract A<T>(C<T>) {
	function f():C2<T> return null;

	static function foo(a:A<String>) return a.f();
}
