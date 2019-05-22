package issues;

class Vec {
    public var x:Float;
	public var y:Float;
    public inline function new(x) {
        this.x = x;
		this.y = x;
    }
}

class Issue6229 {
    static inline function mkVec(x) {
		{
			{
				return new Vec(x);
			}
		}
	}

	@:js('var a = 2;')
	@:analyzer(no_local_dce)
    static function test() {
        var v = mkVec(1);
		var a = v.x + v.y;
    }
}