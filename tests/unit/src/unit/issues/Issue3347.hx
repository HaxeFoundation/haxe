package unit.issues;

private abstract IntMap<V>(Dynamic<V>) {
    public function new() this = {};
    @:arrayAccess public function get(k:IntKey):V return Reflect.field(this, k);
    @:arrayAccess public function set(k:IntKey, i:V):Void Reflect.setField(this, k, i);
}

private abstract IntMap2<V>(Dynamic<V>) {
    public function new() this = {};
    @:arrayAccess public function get<T:IntKey>(k:T):V return Reflect.field(this, cast k);
    @:arrayAccess public function set<T:IntKey>(k:T, i:V):Void Reflect.setField(this, cast k, i);
}

abstract IntMap3<V>(Dynamic<V>) {
    static public var called = false;
	public function new() this = {};
    @:arrayAccess function get(k:IntKey):V return {
		called = true;
		Reflect.field(this, k);
	}
    @:arrayAccess function getInt(k:Int):V return Reflect.field(this, Std.string(k));
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

		t(unit.TestType.typeError(m["1"] = 1));
		t(unit.TestType.typeError(m[1] = "1"));

        var m2 = new IntMap2();
		// should fail because constraints unify without casts
		t(unit.TestType.typeError(m2[1]));

        var m3 = new IntMap3();
        var v = m3[0];
		t(IntMap3.called);
	}
}