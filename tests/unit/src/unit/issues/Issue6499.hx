package unit.issues;

class Issue6499 extends unit.Test {
	function test() {
		#if !macro
		t(Macro.test());
		#end
	}
}

private class Macro {
	macro static public function test() {
		var b = try {
			haxe.macro.Context.getType('any.string.which.ends.with.dot.');
			false;
		} catch(e:Dynamic) {
			true;
		}
		return macro $v{b};
	}
}