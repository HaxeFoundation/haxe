package unit.issues;

private abstract IntMap<V>(Dynamic<V>) {
    public function new() this = {};
    @:arrayAccess public function get(k:IntKey):V return Reflect.field(this, k);
    @:arrayAccess public function set(k:IntKey, i:V):Void Reflect.setField(this, k, i);
}

private abstract IntKey(String) to String {
	static public var fromList = "";
	public inline function new(s) this = s;
    @:from static function fromInt(i:Int):IntKey {
		fromList += i + ";";
		return new IntKey(Std.string(i));
	}
	@:to function toInt() {
		return Std.parseInt(this);
	}
}

class Issue3347 extends Test {
	function test() {
        var m = new IntMap();
		m[0] = 1;
		eq("0;", IntKey.fromList);
		eq(1, m[0]);
		eq("0;0;", IntKey.fromList);
		m[0] += 1;
		eq("0;0;0;", IntKey.fromList);
		eq(2, m[0]);
		m[0] += ((2 : IntKey) : Int);
		eq("0;0;0;0;0;2;", IntKey.fromList);
		eq(4, m[0]);
	}
}