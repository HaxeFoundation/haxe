package unit.issues;

private abstract A(Int) from Int {
	@:op(A++)
	function preInc():Void;

	@:op(++A)
	function postInc():Void;
}

macro function check() {
	#if macro
	final type = haxe.macro.ComplexTypeTools.toType(TPath({
		pack: ["unit", "issues"],
		name: "Issue10641",
		sub: "A"
	}));
	var acc = [];
	function add(s:String) {
		acc.push(s);
	}
	switch type {
		case TAbstract(_.get() => t, params):
			final unop = t.unops[0];
			for (unop in t.unops) {
				add("" + unop.op);
				add("" + unop.postFix);
				add(unop.field.name);
			}
		case _:
	}
	return macro $v{acc.join(" ")};
	#end
}

class Issue10641 extends unit.Test {
	public function test() {
		eq("OpIncrement true preInc OpIncrement false postInc", check());
	}
}
