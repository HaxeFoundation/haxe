package unit.issues;

class Issue4313 extends Test {
	function test():Void {
		aeq(["myMeta1", "myMeta2", "myMeta3"], m("fun"));
	}

	static function fun(@myMeta1 @myMeta2 a:Int, @myMeta3 b:String, c:Bool) {}

	static macro function m(name:String) {
		var cl = haxe.macro.Context.getLocalClass().get();
		for (f in cl.statics.get()) {
			if (f.name == name) switch (f.expr().expr) {
				case TFunction({args: args}):
					var result = [];
					for (arg in args) {
						for (entry in arg.v.meta.get())
							result.push(entry.name);
					}
					return macro $v{result};
				default:
			}
		}
		haxe.macro.Context.error("no such method: " + name, cl.pos);
		return null;
	}
}
