import haxe.macro.Context;

class Rec {
	static var caught = false;

	// each call types a new call of itself, nested in this one
	public static macro function m() {
		try {
			Context.typeof(macro Rec.m());
		} catch (e:Dynamic) {
			if (!caught) {
				caught = true;
				Sys.println(Std.string(e));
			}
		}
		return macro null;
	}
}
