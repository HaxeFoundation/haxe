class A {
}

class B extends A {
	function new() {
		init();
		Test.calls.push("B");
	}
	function init() {}
}

class C extends B {}

class D extends C {
	var n = "D";
	override function init() Test.calls.push(n);
}

class E extends D {
	function new() {
		Test.use(this);
		Test.calls.push("E");
		super();
	}
}

class F extends E {
	public function new() {
		super();
		Test.calls.push("F");
	}
}

extern class ExtNoCtor {
	static function __init__():Void haxe.macro.Compiler.includeFile("./extern.js", "top");
}

class Base extends ExtNoCtor {
	function new() {
		Test.calls.push("BASE");
	}
}

class Child extends Base {
	public function new() {
		Test.use(this);
		Test.calls.push("CHILD");
		super();
	}
}

class RootNoArgs {
	public function new() {}
}

class ChildOneArg extends RootNoArgs {
	public var x:Int;
	public function new(x) {
		super();
		this.x = x;
	}
}

class GrandChildNoArgs extends ChildOneArg {
	public function new() {
		Test.use(this);
		super(42);
	}
}

class Issue9426_1 {
	static function __init__() {
		var sameName = Std.random(10);
		Test.use(sameName);
		Test.use(sameName);
	}
}

class Issue9426_2 {
	static function __init__() {
		var sameName = Std.random(10);
		Test.use(sameName);
		Test.use(sameName);
	}
}

class Test {
	public static var calls:Array<String>;
	@:pure(false) public static function use(v:Any) {}

	static inline final hxCtor = "_hx_constructor";
	static inline final hxSkipCtor = "_hx_skip_constructor";

	static function hasCtorMethod(c:Dynamic):Bool {
		return c.prototype.hasOwnProperty(hxCtor);
	}

	static function hasSkip(c:Dynamic) {
		return c.hasOwnProperty(hxSkipCtor);
	}

	static var failures = 0;

	static function assert(bool:Bool, ?pos:haxe.PosInfos) {
		if (!bool) {
			(untyped process).stderr.write(haxe.Log.formatOutput("assertion failed\n", pos));
			failures++;
		}
	}

	static function main() {
		// A has no constructor, so it's safe to just call `super` first
		// even if we skip ctor in the subclasses
		assert(!hasCtorMethod(A));
		assert(!hasSkip(A));

		// B has an extracted ctor and also the static skip flag,
		// as it is the first in the hierarchy who's ctor needs to be skipped
		assert(hasCtorMethod(B));
		assert(hasSkip(B));

		// C and others down the chain should NOT have the static skip flag,
		// but it can have a hxctor (although in this case it could be optimized away)
		assert(!hasSkip(C));

		// D must have a hxctor because it accesses this before super (for field init)
		// it also supports skipping because it has children who want to skip
		assert(hasCtorMethod(D));
		assert(!hasSkip(D));

		// E also must have a hxctor because it accesses this before super
		// but it doesn't support skipping, because noone down the inheritance requires it
		assert(hasCtorMethod(E));
		assert(!hasSkip(E));

		// F should NOT have hxctor because it doesn't access this before super and
		// it has no children that require skipping
		assert(!hasCtorMethod(F));
		assert(!hasSkip(F));

		// now for the call order
		calls = [];
		new F();
		// E is first, because it's called before `super` in the parent class, and we call `super` before anything else
		// D is second, because we call `init` before pushing `B` in the first constructor. here it's important that the field `n` is set
		// before we call super(), so it's initialized at the point we call `init`
		// B is the last "super" ctor in the hierarchy
		// and finally F is pushed after a super call as usual
		assert(calls.join("") == "EDBF");


		// we also support skipping when inheriting from extern without constructors
		assert(!hasCtorMethod(ExtNoCtor));
		assert(!hasSkip(ExtNoCtor));

		assert(hasCtorMethod(Base));
		assert(hasSkip(Base));

		assert(hasCtorMethod(Child));
		assert(!hasSkip(Child));

		calls = [];
		new Child();
		assert(calls.join("|") == "CHILD|BASE");

		assert(new ChildOneArg(42).x == 42); // #7988

		// ---

		(untyped process).exit(if (failures == 0) 0 else 1);
	}
}
