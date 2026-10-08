package cases.display.issues;

class Issue13061 extends DisplayTestCase {
	function put(name:String, code:String):Markers {
		var markers = Markers.parse(code);
		vfs.putContent(name, markers.source);
		return markers;
	}

	@:coroutine function completionFieldNames(marker:Int):Array<String> {
		var r = runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(marker), wasAutoTriggered: false});
		return [for (i in r.items) if (i.kind == ClassField) i.args.field.name];
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
	function testFailingBuildMacroDoesNotCascadeInCompilation(_) {
		putFailingBuildMacro();
		put("Util.hx", "@:build(M.build())
class Util {
	public var x:Unknown;
}");
		runHaxe(["-main", "Main", "--no-output", "-js", "no.js"]);
		assertErrorMessage("Build boom");
		Assert.isFalse(hasErrorMessage("Type not found : Unknown"));
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
			{code: "class Util extends missing.Pkg {}", error: "Type not found : missing.Pkg"},
			{code: "class Util extends Other.Missing {}", error: "Module Other does not define type Missing"},
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
		enum Broken {
			A(x:Unknown);
		}

		class Dummy {
			public function f(x:Unknown) {}
		}

		function main() {
			new Target();
		}
	**/
	function testDiagnosticsIgnoreMissingTypesInOtherFiles(_) {
		var target = put("Target.hx", "class Target {
	public function new() {}
	function later() {
		var a:{-1-}QqqLater;
	}
}");
		var files = runHaxeJson(["-main", "Main"], DisplayMethods.Diagnostics, {file: new FsPath("Target.hx")});
		Assert.same([target.pos(1)], [for (f in files) for (d in f.diagnostics) d.range.start]);
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
			new Child();
		}
	**/
	function testDiagnosticsMissingSuperclassDoesNotCascade(_) {
		put("Child.hx", "class Child extends missing.Pkg {
	public function new() {
		super();
		super.foo();
	}
}");
		var files = runHaxeJson(["-main", "Main"], DisplayMethods.Diagnostics, {file: new FsPath("Child.hx")});
		Assert.same(["Type not found : missing.Pkg"], [for (f in files) for (d in f.diagnostics) d.args]);
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
			new Main{-1-}HL();
		}
	**/
	function testBuildMacroHostImportsBrokenModule(_) {
		put("Obj.hx", "@:build(Init.init())
@:autoBuild(Init.build())
interface Obj {}");
		put("Init.hx", "import Comps.CustomParser;
class Init {
	public static function init() { return null; }
	public static function build() { return null; }
}");
		put("Comps.hx", "class CustomParser extends missing.CssValue.ValueParser {}
#if !macro
class ObjectComp implements Obj {}
#end");
		var hl = put("MainHL.hx", "package;
class {-1-}MainHL{-2-} {
	public function new() {}
	var f : Comps.ObjectComp;
}");
		Assert.same(hl.range(1, 2), position(1));
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
			var x = M.pick();
			x.f{-1-}oo();
			var y = M.pickOk();
			y.b{-2-}ar();
		}
	**/
	function testResolveTypeWithMissingTypeParameterStillFailsInMacro(_) {
		put("M.hx", "import haxe.macro.Context;
import haxe.macro.Expr;
class M {
	public static macro function pick():Expr {
		return try {
			Context.resolveType(macro : Array<Other.Missing>, Context.currentPos());
			macro new A();
		} catch (e:Dynamic) {
			macro new B();
		}
	}
	public static macro function pickOk():Expr {
		return try {
			Context.resolveType(macro : Array<Other>, Context.currentPos());
			macro new C();
		} catch (e:Dynamic) {
			macro new D();
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
		var c = put("C.hx", "class C {
	public function new() {}
	public function {-3-}bar{-4-}() {}
}");
		put("D.hx", "class D {
	public function new() {}
	public function bar() {}
}");
		Assert.same(b.range(1, 2), position(1));
		Assert.same(c.range(3, 4), position(2));
	}

	/**
		function main() {
			var x = M.pick('Broken');
			x.f{-1-}oo();
			var y = M.pick('Fine');
			y.b{-2-}ar();
		}
	**/
	function testGetTypeWithBrokenFieldStillFailsInMacro(_) {
		put("M.hx", "import haxe.macro.Context;
import haxe.macro.Expr;
class M {
	public static macro function pick(name:String):Expr {
		return try {
			Context.getType(name);
			macro new C();
		} catch (e:Dynamic) {
			macro new B();
		}
	}
}");
		put("Other.hx", "class Other {}");
		put("Broken.hx", "class Broken {
	public var x:Other.Missing;
}");
		put("Fine.hx", "class Fine {
	public var x:Int;
}");
		var b = put("B.hx", "class B {
	public function new() {}
	public function {-1-}foo{-2-}() {}
}");
		var c = put("C.hx", "class C {
	public function new() {}
	public function {-3-}bar{-4-}() {}
}");
		Assert.same(b.range(1, 2), position(1));
		Assert.same(c.range(3, 4), position(2));
	}

	/**
		function main() {
			var c = new Child();
			c.d{-1-}yn = 1;
		}
	**/
	function testBuildMacroDoesNotSeeDynamicForMissingType(_) {
		put("M.hx", "import haxe.macro.Context;
import haxe.macro.Expr;
import haxe.macro.Type;
class M {
	public static function build():Array<Field> {
		var fields = Context.getBuildFields();
		var name = try {
			switch Context.getLocalClass().get().superClass.t.get().fields.get()[0].type {
				case TDynamic(_): 'dyn';
				default: 'ok';
			}
		} catch (e:Dynamic) 'failed';
		fields.push({name: name, access: [APublic], pos: Context.currentPos(), kind: FVar(macro : Int)});
		return fields;
	}
}");
		put("Obj.hx", "@:autoBuild(M.build())
interface Obj {}");
		put("Base.hx", "class Base {
	public var x:Other.Missing;
	public function new() {}
}");
		put("Other.hx", "class Other {}");
		put("Child.hx", "class Child extends Base implements Obj {}");
		Assert.same([], runHaxeJson([], DisplayMethods.GotoDefinition, {file: file, offset: offset(1)}));
	}

	/**
		function main() {
			var b = new Base();
			b.x;
			var c = new Child();
			c.d{-1-}yn = 1;
		}
	**/
	function testBuildMacroDoesNotSeeDynamicForMissingTypeForcedEarlier(_) {
		put("M.hx", "import haxe.macro.Context;
import haxe.macro.Expr;
import haxe.macro.Type;
class M {
	public static function build():Array<Field> {
		var fields = Context.getBuildFields();
		var name = try {
			switch Context.getLocalClass().get().superClass.t.get().fields.get()[0].type {
				case TDynamic(_): 'dyn';
				default: 'ok';
			}
		} catch (e:Dynamic) 'failed';
		fields.push({name: name, access: [APublic], pos: Context.currentPos(), kind: FVar(macro : Int)});
		return fields;
	}
}");
		put("Obj.hx", "@:autoBuild(M.build())
interface Obj {}");
		put("Base.hx", "class Base {
	public var x:Other.Missing;
	public function new() {}
}");
		put("Other.hx", "class Other {}");
		put("Child.hx", "class Child extends Base implements Obj {}");
		Assert.same([], runHaxeJson([], DisplayMethods.GotoDefinition, {file: file, offset: offset(1)}));
	}

	/**
		function main() {
			var c = new Child();
			c.{-1-}
		}
	**/
	function testBuildMacroDoesNotSeeDynamicForBrokenDeclarations(_) {
		put("M.hx", "import haxe.macro.Context;
import haxe.macro.Expr;
import haxe.macro.Type;
class M {
	static function dyn(t:Type):Bool {
		return switch t {
			case TDynamic(_): true;
			case TFun(args, ret): dyn(ret) || Lambda.exists(args, a -> dyn(a.t));
			default: false;
		}
	}
	static function inspect(t:Type):Bool {
		return switch t {
			case TInst(c, _): dyn(c.get().fields.get()[0].type);
			case TEnum(e, _): Lambda.exists([for (c in e.get().constructs) c], c -> dyn(c.type));
			case TType(t, _): dyn(t.get().type);
			case TAbstract(a, _): dyn(a.get().type);
			default: dyn(t);
		}
	}
	public static function build():Array<Field> {
		var fields = Context.getBuildFields();
		try {
			var sup = Context.getLocalClass().get().superClass.t.get();
			for (f in sup.fields.get().concat([sup.constructor.get()]))
				if (inspect(f.type))
					fields.push({name: 'dyn_' + f.name, access: [APublic], pos: Context.currentPos(), kind: FVar(macro : Int)});
		} catch (e:Dynamic) {}
		fields.push({name: 'ran', access: [APublic], pos: Context.currentPos(), kind: FVar(macro : Int)});
		return fields;
	}
}");
		put("Obj.hx", "@:autoBuild(M.build())
interface Obj {}");
		put("Other.hx", "class Other {}");
		put("BrokenClass.hx", "class BrokenClass { public var x:Other.Missing; }");
		put("BrokenEnum.hx", "enum BrokenEnum { A(x:Other.Missing); }");
		put("BrokenAbstract.hx", "abstract BrokenAbstract(Other.Missing) {}");
		put("BrokenTypedef.hx", "typedef BrokenTypedef = Array<Other.Missing>;");
		put("Base.hx", "class Base {
	public var c:BrokenClass;
	public var e:BrokenEnum;
	public var a:BrokenAbstract;
	public var t:BrokenTypedef;
	public function new(x:Other.Missing) {}
	public function m(p:Other.Missing):Void {}
}");
		put("Child.hx", "class Child extends Base implements Obj {
	public function new() { super(null); }
}");
		var r = runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var names = [for (i in r.items) if (i.kind == ClassField) i.args.field.name];
		Assert.contains("ran", names);
		Assert.same([], names.filter(n -> n.indexOf("dyn_") == 0));
	}

	/**
		function main() {
			var a = new A();
			a.hel{-1-}lo();
			var b = new B();
			b.hel{-2-}lo();
			var c = new C();
			c.hel{-3-}lo();
		}
	**/
	function testSuperclassFromMissingLibrary(_) {
		put("Other.hx", "class Other {}");
		var a = put("A.hx", "class A extends missing.Pkg {
	public function new() {}
	public function {-1-}hello{-2-}() {}
}");
		var b = put("B.hx", "class B extends Other.Missing {
	public function new() {}
	public function {-1-}hello{-2-}() {}
}");
		var c = put("C.hx", "class C extends A {
	public function new() { super(); }
	public function {-1-}hello{-2-}() {}
}");
		Assert.same(a.range(1, 2), position(1));
		Assert.same(b.range(1, 2), position(2));
		Assert.same(c.range(1, 2), position(3));
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

	/**
		function main() {
			var c = new Util();
			c.{-1-}
		}
	**/
	function testCaughtResolveTypeFailureDoesNotBreakLaterMacroReads(_) {
		put("M.hx", "import haxe.macro.Context;
import haxe.macro.Expr;
class M {
	public static function build():Array<Field> {
		var fields = Context.getBuildFields();
		try Context.resolveType(macro : Other.Missing, Context.currentPos()) catch (e:Dynamic) {}
		var r = try { Context.getLocalClass().get().name; 'read'; } catch (e:Dynamic) 'raised';
		fields.push({name: r, access: [APublic], pos: Context.currentPos(), kind: FVar(macro : Int)});
		return fields;
	}
}");
		put("Other.hx", "class Other {}");
		put("Util.hx", "@:build(M.build())
class Util {
	public function new() {}
}");
		var r = runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var names = [for (i in r.items) if (i.kind == ClassField) i.args.field.name];
		Assert.contains("read", names);
		Assert.notContains("raised", names);
	}

	/**
		function main() {
			var c = new Child();
			c.{-1-}dyn_x{-2-} = 1;
			c.ran = 1;
		}
	**/
	function testDiagnosticsMacroDoesNotSeeDynamicForUnqualifiedMissingType(_) {
		put("M.hx", "import haxe.macro.Context;
import haxe.macro.Expr;
class M {
	public static function build():Array<Field> {
		var fields = Context.getBuildFields();
		try {
			for (f in Context.getLocalClass().get().superClass.t.get().fields.get())
				switch f.type {
					case TDynamic(_): fields.push({name: 'dyn_' + f.name, access: [APublic], pos: Context.currentPos(), kind: FVar(macro : Int)});
					default:
				}
		} catch (e:Dynamic) {}
		fields.push({name: 'ran', access: [APublic], pos: Context.currentPos(), kind: FVar(macro : Int)});
		return fields;
	}
}".replace("switch f.type", "switch f.type"));
		put("Obj.hx", "@:autoBuild(M.build())
interface Obj {}");
		put("Base.hx", "class Base {
	public var x:Missing;
	public function new() {}
}");
		put("Child.hx", "class Child extends Base implements Obj {}");
		var files = runHaxeJson(["-main", "Main"], DisplayMethods.Diagnostics, {file: file});
		var ranges = [for (f in files) for (d in f.diagnostics) d.range];
		var expected = range(1, 2);
		Assert.isTrue(ranges.exists(r -> r.start.line == expected.start.line && r.start.character == expected.start.character));
	}

	/**
		function main() {
			var x:Other.Missing = null;
			var v = M.names();
			v.{-1-}
		}
	**/
	function testMacroLocalVarsDoNotSeeDynamicForMissingType(_) {
		put("M.hx", "import haxe.macro.Context;
import haxe.macro.Expr;
import haxe.macro.Type;
class M {
	public static macro function names():Expr {
		var dyn = false;
		for (t in Context.getLocalVars()) switch t { case TDynamic(_): dyn = true; default: }
		return dyn ? macro new Dyn() : macro new Ok();
	}
}");
		put("Other.hx", "class Other {}");
		put("Dyn.hx", "class Dyn { public function new() {} public function sawDynamic() {} }");
		put("Ok.hx", "class Ok { public function new() {} public function sawNothing() {} }");
		var r = runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var names = [for (i in r.items) if (i.kind == ClassField) i.args.field.name];
		Assert.notContains("sawDynamic", names);
	}

	/**
		function main() {
			var c = new B();
			c.{-1-}
		}
	**/
	function testBuildMacroOfSiblingClassStillWorks(_) {
		put("M.hx", "import haxe.macro.Context;
import haxe.macro.Expr;
class M {
	public static function build():Array<Field> {
		var fields = Context.getBuildFields();
		var r = try { Context.getLocalClass().get().name; 'read'; } catch (e:Dynamic) 'raised';
		fields.push({name: r, access: [APublic], pos: Context.currentPos(), kind: FVar(macro : Int)});
		return fields;
	}
}");
		put("Other.hx", "class Other {}");
		put("B.hx", "class A {
	public var x:Other.Missing;
	public function new() {}
}

@:build(M.build())
class B {
	public var a:A;
	public function new() {}
}");
		var r = runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var names = [for (i in r.items) if (i.kind == ClassField) i.args.field.name];
		Assert.contains("read", names);
	}

	/**
		function main() {
			var c = new Child();
			c.{-1-}
		}
	**/
	function testMacroDoesNotSeeDynamicWhenFieldTypeIsForcedByTheMacro(_) {
		put("M.hx", "import haxe.macro.Context;
import haxe.macro.Expr;
import haxe.macro.Type;
class M {
	public static function build():Array<Field> {
		var fields = Context.getBuildFields();
		switch Context.getType('Holder') {
			case TInst(c, _):
				var name = switch c.get().fields.get()[0].type {
					case TDynamic(_): 'sawDynamic';
					default: 'ran';
				}
				fields.push({name: name, access: [APublic], pos: Context.currentPos(), kind: FVar(macro : Int)});
			default:
		}
		return fields;
	}
}");
		put("Other.hx", "class Other { public var y:Int; }");
		put("Child.hx", "@:build(M.build())
class Child {
	public var h:Holder;
	public function new() {}
}");
		put("Holder.hx", "class Holder {
	public var x:Other.Missing;
	public var c:Child;
	public function new() {}
}");
		Assert.notContains("sawDynamic", completionFieldNames(1));
	}

	/**
		function main() {
			var c = new Child();
			c.{-1-}
		}
	**/
	function testMacroReadsFieldTypeWithoutErrors(_) {
		put("M.hx", "import haxe.macro.Context;
import haxe.macro.Expr;
import haxe.macro.Type;
class M {
	public static function build():Array<Field> {
		var fields = Context.getBuildFields();
		switch Context.getType('Holder') {
			case TInst(c, _):
				var name = switch c.get().fields.get()[0].type {
					case TDynamic(_): 'sawDynamic';
					default: 'ran';
				}
				fields.push({name: name, access: [APublic], pos: Context.currentPos(), kind: FVar(macro : Int)});
			default:
		}
		return fields;
	}
}");
		put("Other.hx", "class Other { public var y:Int; }");
		put("Child.hx", "@:build(M.build())
class Child {
	public var h:Holder;
	public function new() {}
}");
		put("Holder.hx", "class Holder {
	public var x:Int;
	public var c:Child;
	public function new() {}
}");
		Assert.contains("ran", completionFieldNames(1));
	}
}
