import haxe.macro.Context;
import haxe.macro.Expr;

using haxe.macro.Tools;

abstract Foo(Dynamic) from Dynamic to Dynamic {
	@:op(A.B) function get(prop:String) {
		return Reflect.field(this, prop);
	}

	@:op(A.B) macro function set(obj:Expr, prop:Expr, val:Expr) {
		var s = Context.getTypedExpr(Context.typeExpr(obj)).toString();
		Sys.stderr().writeString(s);
		return macro $val;
	}
}

class Main {
	static function main() {
		var foo:Foo = {
			n: 5
		};
		foo.n += 4;
	}
}
