package unit.issues;

class Issue7007 extends unit.Test {
	function test() {
		#if !macro
		eq("Exception caught: Invalid expression", catchMe());
		#end
	}

	macro static function catchMe() {
		try {
			trace(haxe.macro.Context.getTypedExpr(null));
		} catch (e:Dynamic) {
			return macro $v{'Exception caught: $e'};
		}
		return macro null;
	}
}