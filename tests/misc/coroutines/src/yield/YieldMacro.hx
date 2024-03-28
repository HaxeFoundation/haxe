package yield;

import haxe.macro.Context;
import haxe.macro.Expr;
import haxe.macro.Printer;
using Lambda;
using haxe.macro.Tools;

class YieldMacro {
	macro static public function build():Array<Field> {
		var yieldFunctions = [];
		var otherFunctions = [];
		var inputFields = Context.getBuildFields();
		for (field in inputFields) {
			if (field.meta.exists(meta -> meta.name == ":yield")) {
				var f = switch (field.kind) {
					case FFun(f):
						f;
					case _:
						Context.error("@:yield fields should be functions, found " + field.kind, field.pos);
				}
				transformYieldFunction(f, field.pos);
				yieldFunctions.push(field);
			}
		}
		return inputFields;
	}

	static function transformYieldFunction(f:Function, p:Position) {
		if (f.expr == null) {
			Context.error('@:yield function has no expression', p);
		}
		var ret = switch (f.ret) {
			case macro :Iterator<$ct>:
				macro : Coroutine<$ct -> Void>;
			case _:
				null;
		}
		function mapYield(e:Expr) {
			return switch (e) {
				case macro @:yield return $e:
					e = mapYield(e);
					macro @:pos(e.pos) yield($e);
				case macro @:yield $e:
					switch (e.expr) {
						case EFunction(kind, f):
							transformYieldFunction(f, e.pos);
							e;
						case _:
							e.map(mapYield);
					}
				case _:
					e.map(mapYield);
			}
		}
		var e = mapYield(f.expr);
		e = macro return sequence((yield : $ret) -> $e);
		// trace(new Printer().printExpr(e));
		f.expr = e;
	}
}