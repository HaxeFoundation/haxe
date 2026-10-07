package cases.display.issues;

class Issue13061 extends DisplayTestCase {
	function put(name:String, code:String):Markers {
		var markers = Markers.parse(code);
		vfs.putContent(name, markers.source);
		return markers;
	}

	function putFailingBuildMacro() {
		put("M.hx", "import haxe.macro.Context;
import haxe.macro.Expr;
class M {
	public static function build():Array<Field> {
		Context.error('Build boom', Context.currentPos());
		return null;
	}
}");
	}

	/**
		function main() {
			var u:Util = null;
		}
	**/
	function testCompilationStillFails(_) {
		putFailingBuildMacro();
		put("Other.hx", "class Other {}\nprivate class Hidden {}");
		for (broken in [
			{code: "class Util { public static function foo(x:Unknown):Int return 1; }", error: "Type not found : Unknown"},
			{code: "class Util { public static function foo(x:Other.Missing):Int return 1; }", error: "Module Other does not define type Missing"},
			{code: "class Util { public static function foo(x:Other.Hidden):Int return 1; }", error: "Cannot access private type Hidden in module Other"},
			{code: "class Util { public function new(x:Unknown) {} }", error: "Type not found : Unknown"},
			{code: "typedef Css = missing.CssValue;\nclass Util {}", error: "Type not found : missing.CssValue"},
			{code: "enum Util { A(x:Unknown); B; }", error: "Type not found : Unknown"},
			{code: "abstract Util(Unknown) {}", error: "Type not found : Unknown"},
			{code: "@:build(M.build())\nclass Util {}", error: "Build boom"}
		]) {
			put("Util.hx", broken.code);
			runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Util.hx")});
			runHaxe(["-main", "Main", "--no-output", "-js", "no.js"]);
			assertErrorMessage(broken.error);
		}
	}

	/**
		function main() {
			Util.foo(1);
		}
	**/
	function testDiagnosticsReportsAllMissingTypes(_) {
		put("Other.hx", "class Other {}\nprivate class Hidden {}");
		var util = put("Util.hx", "class Util {
	public static function foo(x:{-1-}Other.Missing):Int { return 1; }
	public static function bar(x:{-2-}Other.Hidden):Int { return 1; }
	public static function baz(x:{-3-}Unknown):Int { return 1; }
	public static function qux(x:{-4-}missing.Pkg):Int { return 1; }
}");
		var files = runHaxeJson(["-main", "Main"], DisplayMethods.Diagnostics, {file: new FsPath("Util.hx")});
		var starts = [for (f in files) for (d in f.diagnostics) d.range.start];
		starts.sort((a, b) -> a.line - b.line);
		Assert.same([for (n in 1...5) util.pos(n)], starts);
	}

	/**
		function main() {
			Util.f{-1-}oo(1);
			Util.b{-2-}ar();
		}
	**/
	function testUnknownTypeInSignature(_) {
		var util = put("Util.hx", "class Util {
	public static function {-1-}foo{-2-}(x:Unknown):Int { return 1; }
	public static function {-3-}bar{-4-}() { return 1; }
}");
		Assert.same(util.range(1, 2), position(1));
		Assert.same(util.range(3, 4), position(2));
	}

	/**
		function main() {
			n{-1-}ew Util(1);
		}
	**/
	function testUnknownTypeInConstructor(_) {
		var util = put("Util.hx", "class Util {
	public function {-1-}new{-2-}(x:Unknown) {}
}");
		Assert.same(util.range(1, 2), position(1));
	}

	/**
		function main() {
			var i = new comp.In{-1-}put(1);
			i.te{-2-}xt = "a";
			i.onC{-3-}hange();
		}
	**/
	function testSuperclassWithMissingLibraryTypes(_) {
		put("comp/Base.hx", "package comp;
import missing.Property;
typedef Css = missing.CssValue;
class Base {
	public function new() {}
	public function baseFn(x:Css):Int { return 1; }
}");
		var input = put("comp/Input.hx", "package comp;
class {-1-}Input{-2-} extends Base {
	public var {-3-}text{-4-}:String;
	public function new(a:Int) { super(); }
	public function {-5-}onChange{-6-}() {}
}");
		Assert.same(input.range(1, 2), position(1));
		Assert.same(input.range(3, 4), position(2));
		Assert.same(input.range(5, 6), position(3));
	}

	/**
		function main() {
			Built.f{-1-}oo();
		}
	**/
	function testFailingBuildMacro(_) {
		putFailingBuildMacro();
		var built = put("Built.hx", "@:build(M.build())
class Built {
	public static function {-1-}foo{-2-}():Int return 1;
}");
		Assert.same(built.range(1, 2), position(1));
	}

	/**
		import Util;

		function main() {
			Util.f{-1-}oo();
			var b:Bro{-2-}ken = null;
		}
	**/
	function testFailingAbstractInModule(_) {
		var util = put("Util.hx", "class Util {
	public static function {-3-}foo{-4-}():Int return 1;
}

abstract {-1-}Broken{-2-}(Unknown) {
	public function new() {}
}");
		Assert.same(util.range(3, 4), position(1));
		Assert.same(util.range(1, 2), position(2));
	}

	/**
		import Util;

		function main() {
			Util.f{-1-}oo();
			var b = Broken.{-2-}B;
			var a = Broken.{-3-}A(1);
		}
	**/
	function testFailingEnumInModule(_) {
		var util = put("Util.hx", "enum Broken {
	{-5-}A{-6-}(x:Unknown);
	{-1-}B{-2-};
}

class Util {
	public static function {-3-}foo{-4-}():Int return 1;
}");
		Assert.same(util.range(3, 4), position(1));
		Assert.same(util.range(1, 2), position(2));
		Assert.same(util.range(5, 6), position(3));
	}

	/**
		function main() {
			Util.f{-1-}oo(1);
			Util.b{-2-}ar(1);
		}
	**/
	function testTypeNotDefinedInExistingModule(_) {
		put("Other.hx", "class Other {}\nprivate class Hidden {}");
		var util = put("Util.hx", "class Util {
	public static function {-1-}foo{-2-}(x:Other.Missing):Int { return 1; }
	public static function {-3-}bar{-4-}(x:Other.Hidden):Int { return 1; }
}");
		Assert.same(util.range(1, 2), position(1));
		Assert.same(util.range(3, 4), position(2));
	}

	/**
		function main() {
			var x = M.pick();
			x.f{-1-}oo();
		}
	**/
	function testResolveTypeStillFailsInMacro(_) {
		put("M.hx", "import haxe.macro.Context;
import haxe.macro.Expr;
class M {
	public static macro function pick():Expr {
		return try {
			Context.resolveType(macro : Other.Missing, Context.currentPos());
			macro new A();
		} catch (e:Dynamic) {
			macro new B();
		}
	}
}");
		put("Other.hx", "class Other {}");
		put("A.hx", "class A {
	public function new() {}
	public function foo() {}
}");
		var b = put("B.hx", "class B {
	public function new() {}
	public function {-1-}foo{-2-}() {}
}");
		Assert.same(b.range(1, 2), position(1));
	}

	/**
		function main() {
			var c = new Child();
			c.hel{-1-}lo();
		}
	**/
	function testBrokenSuperclass(_) {
		var child = put("Child.hx", "class Child extends Missing {
	public function new() {}
	public function {-1-}hello{-2-}() {}
}");
		Assert.same(child.range(1, 2), position(1));
	}

	/**
		import Util;

		function main() {
			Util.f{-1-}oo(1);
			var e = E.{-2-}B;
		}
	**/
	function testBreakingAndFixingKeepsCacheIntact(_) {
		var args = ["-main", "Main", "--no-output", "-js", "no.js"];
		var util = type -> put("Util.hx", "class Util {
	public static function {-1-}foo{-2-}(x:" + type + "):Int { return 1; }
}
enum E { A(x:" + type + "); {-3-}B{-4-}; }");
		util("Int");
		runHaxe(args);
		assertSuccess();
		for (_ in 0...2) {
			var broken = util("Unknown");
			runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Util.hx")});
			Assert.same(broken.range(1, 2), position(1));
			Assert.same(broken.range(3, 4), position(2));
			var fixed = util("Int");
			runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Util.hx")});
			Assert.same(fixed.range(1, 2), position(1));
			Assert.same(fixed.range(3, 4), position(2));
			runHaxe(args);
			assertSuccess();
		}
	}

	/**
		function main() {
			Util.f{-1-}oo(1);
		}
	**/
	function testErrorStillReportedAfterRecovery(_) {
		var util = put("Util.hx", "class Util {
	public static function {-1-}foo{-2-}(x:{-3-}Unknown{-4-}):Int { return 1; }
}");
		Assert.same(util.range(1, 2), position(1));
		var args = ["-main", "Main"];
		var diagnostics = runHaxeJson(args, DisplayMethods.Diagnostics, {file: new FsPath("Util.hx")});
		Assert.same([util.range(3, 4)], [for (f in diagnostics) for (d in f.diagnostics) d.range]);
		runHaxe(args.concat(["--no-output", "-js", "no.js"]));
		assertErrorMessage("Type not found : Unknown");
	}
}
