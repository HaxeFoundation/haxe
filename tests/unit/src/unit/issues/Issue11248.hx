package unit.issues;

class Issue11248 extends unit.Test {
	// TODO: BIT_A should be UInt32 to match the original issue, but typing UInt32 as a Map<Int,Int>
	// value with XOR assignment currently requires an explicit cast. Using Int for now.
	public static var BIT_A:Int = 1;
	public static var FLAG_1:Int = 0;

	static final _flags:Map<Int, Int> = [0 => 1010];
	public static var flags(get, never):Map<Int, Int>;

	public static function get_flags() {
		return _flags;
	}

	function test() {
		flags[FLAG_1] ^= BIT_A;
		eq(1011, flags[FLAG_1]);
	}
}
