@:structInit
class Foo {
	@:isVar public var real(get, set):String;
	public var foo(get, never):String;
	public var bar(get, set):String;

	function get_real()
		return real;

	function set_real(v)
		return real = v;

	function get_foo()
		return "foo";

	function get_bar()
		return "bar";

	function set_bar(v)
		return v;
}

function main() {
	var foo:Foo = {
		real: "real"
	};
}
