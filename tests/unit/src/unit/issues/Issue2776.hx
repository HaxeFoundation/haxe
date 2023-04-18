package unit.issues;

import unit.Test;

class Issue2776 extends Test {
	function test() {
		var type = null;
		switch (type) {
			case String:
			case Int:
		}
		unit.HelperMacros.typedAs(type, getClassT());

		var type = null;
		switch (type) {
			case haxe.macro.Expr.ExprDef:
			case haxe.macro.Expr.Constant:
		}
		unit.HelperMacros.typedAs(type, getEnumT());

		var type = null;
		t(unit.HelperMacros.typeError(switch (type) {
			case String:
			case haxe.macro.Expr.ExprDef:
		}));
	}

	static function getClassT<T>():#if cs Class<T> #else Null<Class<T>> #end {
		return (null : Class<T>);
	}

	static function getEnumT<T>():#if cs Enum<T> #else Null<Enum<T>> #end {
		return (null : Enum<T>);
	}
}
