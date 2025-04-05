package unit.issues.misc;

class Issue9600Macro {
	public static macro function contextTypeofError() {
		try {
			haxe.macro.Context.typeof(macro ({a: 42, b: true} :{a:Int}));
			return macro "exception expected";
		} catch (e:Dynamic) {
			return macro $v{Std.string(e)};
		}
	}
}
