package unit.issues;

class Issue8092 extends Test {
	function test() {
		eq("String", m());
	}

	static macro function m() {
		var caught = try {
			haxe.macro.Context.getType("haxe.Constraints");
			"none";
		} catch (e:String) {
			"String";
		} catch (e:Dynamic) {
			"other";
		}
		return macro $v{caught};
	}
}
