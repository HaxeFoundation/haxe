class A<T> {}

@:nativeGen
class B<T> {}

class C<T1,T2> {
	var a:A<T1>;
	var b:B<T2>;

	function f():A<T1> return null;
}

class D extends C<String, String> {
	override function f():A<String> return null;
}
