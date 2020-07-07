package lua.lib.lpeg;
abstract Matched(Int) {
    inline public function new(d:Dynamic) {
        this = d;
    }
    inline public function matched() : Bool {
        return Std.isOfType(this, Int) || Lua.next(cast this) != null;
    }
    inline public function captures() : Table<String,String> {
        if (Std.isOfType(this,Table)) {
            return cast this;
        } else {
            return Table.create();
        }
    }
    inline public function position() : Int {
        if (Std.isOfType(this, Int)) {
            return this;
        } else {
            return null;
        }
    }
}

