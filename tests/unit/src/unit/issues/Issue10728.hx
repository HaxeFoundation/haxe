package unit.issues;

class Issue10728 extends Test {
	function test(){
		eq("String", m());
	}

	static macro function m() {
		var mono = haxe.macro.Context.makeMonomorph();
		var td = switch (haxe.macro.Context.getType("B")) {
			case TType(td, _): td;
			case _: throw "assert";
		};
		haxe.macro.Context.unify(haxe.macro.Context.getType("A"), TType(td, [mono]));
		return macro $v{haxe.macro.TypeTools.toString(mono)};
	}
}

private abstract A(Int) to B<String> {}

private typedef B<T> = Int;
