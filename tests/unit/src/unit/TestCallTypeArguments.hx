package unit;

using Lambda;

private class Element {
	public function new() {}
}

private class InputElement extends Element {
	public var value = "input";

	public function new() {
		super();
	}
}

private class Document {
	public function new() {}

	public function find<T:Element>(id:String):Null<T> {
		return cast new InputElement();
	}
}

private class Box<T> {
	public var value:T;

	public function new(value:T) {
		this.value = value;
	}

	public function map<U>(f:T->U):Box<U> {
		return new Box(f(value));
	}
}

private abstract Wrap<T>(Array<T>) {
	public function new(value:T) {
		this = [value];
	}

	public function first<U>(f:T->U):U {
		return f(this[0]);
	}

	public static function of<T>(value:T):Wrap<T> {
		return new Wrap(value);
	}
}

class TestCallTypeArguments extends Test {
	static function pair<A, B>(a:A, b:B):{a:A, b:B} {
		return {a: a, b: b};
	}

	function testInstanceCall() {
		var input = new Document().find.<InputElement>("name");
		eq("input", input.value);
		t(HelperMacros.typeString(new Document().find.<InputElement>("name")) == "Null<unit._TestCallTypeArguments.InputElement>");
	}

	function testStaticCall() {
		var p = pair.<Float, Array<Array<Int>>>(1, [[2]]);
		eq("Float", HelperMacros.typeString(p.a));
		eq(2, p.b[0][0]);
	}

	function testStdCall() {
		eq("1,2", [1, 2].map.<String>(i -> Std.string(i)).join(","));
	}

	static function id<T>(x:T):T {
		return x;
	}

	static function apply<A, B>(f:A->B, a:A):B {
		return f(a);
	}

	static function make<T>():Array<T> {
		return [];
	}

	static function outer<T>(x:T) {
		return id.<T>(x);
	}

	@:generic static function generic<T>(x:T):Array<T> {
		return [x];
	}

	static inline function inlined<T>(x:T):T {
		return x;
	}

	static function structural<T:{x:Int}>(v:T):T {
		return v;
	}

	function testInference() {
		eq("Float", HelperMacros.typeString(id.<Float>(1)));
		eq("3", apply.<Int, String>(x -> Std.string(x + 1), 2));
		var a = make.<Int>();
		a.push(1);
		eq("Array<Int>", HelperMacros.typeString(a));
		var f:Float = id.<Int>(1);
		eq(1.0, f);
		eq("String", HelperMacros.typeString(outer("s")));
	}

	function testTypes() {
		eq("Null<Int>", HelperMacros.typeString(id.<Null<Int>>(null)));
		eq("Int -> String", HelperMacros.typeString(id.<Int->String>(Std.string)));
		eq("{ x : Int }", HelperMacros.typeString(id.<{x:Int}>({x: 1})));
		eq("Dynamic", HelperMacros.typeString(id.<Dynamic>("s")));
	}

	function testFieldKinds() {
		eq("1,2", [1, 2].fold.<Int, String>((x, acc) -> acc + x + ",", "").substr(0, 3));
		eq("1", new Box(1).map.<String>(Std.string).value);
		eq("1", new Wrap(1).first.<String>(Std.string));
		eq("Wrap<Float>", HelperMacros.typeString(Wrap.of.<Float>(1)).split(".").pop());
		eq("Array<Float>", HelperMacros.typeString(generic.<Float>(1)));
		eq("Float", HelperMacros.typeString(inlined.<Float>(1)));
		eq(2, structural.<{x:Int, y:Int}>({x: 1, y: 2}).y);
	}

	function testErrors() {
		t(HelperMacros.typeError(pair.<Int>(1, 2)));
		t(HelperMacros.typeError(pair.<String, Int>(1, 2)));
		t(HelperMacros.typeError(new Document().find.<String>("name")));
		t(HelperMacros.typeError(var f = pair.<Int, Int>));
		t(HelperMacros.typeError(structural.<{y:Int}>({y: 1})));
	}
}
