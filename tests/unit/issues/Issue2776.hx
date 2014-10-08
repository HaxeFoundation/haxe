package unit.issues;
import unit.Test;

@:analyzer(no_check_has_effect)
class Issue2776 extends Test {
	function test() {
		var type = null;
		switch(type) {
			case String:
			case Int:
		}
		unit.TestType.typedAs(type, getClassT());

		var type = null;
		switch(type) {
			case haxe.macro.Expr.ExprDef:
			case haxe.macro.Expr.Constant:
		}
		unit.TestType.typedAs(type, getEnumT());

		var type = null;
		t(unit.TestType.typeError(switch(type) {
			case String:
			case haxe.macro.Expr.ExprDef:
		}));
	}

	static function getClassT<T>():Class<T> {
		return (null : Class<T>);
	}

	static function getEnumT<T>():Enum<T> {
		return (null : Enum<T>);
	}
}