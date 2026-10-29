package cases.display.issues;

class Issue13063 extends DisplayTestCase {
	function put(name:String, code:String) {
		vfs.putContent(name, code);
	}

	function putTypeNameMacro() {
		put("M.hx", "import haxe.macro.Context;
import haxe.macro.Expr;
class M {
	public static macro function typeName(e:Expr):Expr {
		return macro $v{haxe.macro.TypeTools.toString(Context.typeof(e))};
	}
}");
	}

	function putUtilWithMissingType() {
		put("Util.hx", "class Util {
	public static function foo(x:Unknown):Int return 1;
}");
	}

	/**
		function main() {
			var x:Int = "foo";
			var s = M.typeName(1);
			s.{-1-}
		}
	**/
	function testTypeofAfterUnifyError(_) {
		putTypeNameMacro();
		Assert.isTrue(hasField(fields(1), "length"));
	}

	/**
		function main() {
			Util.foo(1);
			var s = M.typeName(1);
			s.{-1-}
		}
	**/
	function testTypeofAfterRecoveredMissingType(_) {
		putTypeNameMacro();
		putUtilWithMissingType();
		Assert.isTrue(hasField(fields(1), "length"));
	}

	/**
		function main() {
			Util.foo(1);
			var a:Gen<String> = null;
			a.{-1-}
		}
	**/
	function testGetLocalTypeAfterRecoveredMissingType(_) {
		putUtilWithMissingType();
		put("G.hx", "import haxe.macro.Context;
import haxe.macro.Expr;
class G {
	public static function build():ComplexType {
		return switch Context.getLocalType() {
			case TInst(_, [t]):
				var ct = Context.toComplexType(t);
				macro : Array<$ct>;
			case _:
				throw 'unexpected';
		}
	}
}");
		put("Gen.hx", "@:genericBuild(G.build()) class Gen<T> {}");
		Assert.isTrue(hasField(fields(1), "push"));
	}

	/**
		function main() {
			var x:Int = "foo";
			var s = M.typeName(1);
			trace(s.length);
		}
	**/
	function testDiagnosticsAfterUnifyError(_) {
		putTypeNameMacro();
		var files = runHaxeJson(["-main", "Main"], DisplayMethods.Diagnostics, {file: file});
		Assert.same(["String should be Int"], [for (f in files) for (d in f.diagnostics) d.args]);
	}

	/**
		function main() {
			Util.foo(1);
			var s = M.typeName(1);
			trace(s.length);
		}
	**/
	function testDiagnosticsAfterRecoveredMissingType(_) {
		putTypeNameMacro();
		putUtilWithMissingType();
		var files = runHaxeJson(["-main", "Main"], DisplayMethods.Diagnostics, {file: file});
		Assert.same([], [for (f in files) for (d in f.diagnostics) d.args]);
	}
}
