@RuntimeMeta([42,0])
class Macro {
	public static function init() {
		var m = haxe.rtti.Meta.getType(Macro);
		if (m.RuntimeMeta[0][0] != 42) throw "runtime meta failure";
	}
}
