package unit.issues.misc;

import haxe.macro.Context;
import haxe.macro.Expr;

class Issue10259Macro {
	macro static public function getMaps() {
		final p = Context.currentPos();
		final intMap = Context.makeExpr([1 => 2], p);
		final stringMap = Context.makeExpr(["1" => 2], p);
		final objectMap = Context.makeExpr([
			{x: 1}
			=> 2
		], p);

		return macro {
			intMap: $intMap,
			stringMap: $stringMap,
			objectMap: $objectMap
		}
	}
}
