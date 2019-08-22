package unit.issues;

class Issue8261 extends unit.Test {
	function test() {
		#if !macro
		var ct = getComplexType();
		var name = switch (ct) {
			case TPath(path): path.sub;
			case _: null;
		}
		eq("Int", name);
		#end
	}

    macro static function getComplexType() {
        var ct = haxe.macro.Context.toComplexType(haxe.macro.Context.getType("Int"));
        return macro $v{ct};
    }
}