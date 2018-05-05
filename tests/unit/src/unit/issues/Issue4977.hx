package unit.issues;

private abstract Progress(Int) to Int {
	public var isEof(get, never):Bool;

	inline function get_isEof()
		return this < 0;

	public var bytes(get, never):Int;

	inline function get_bytes():Int
		return (this < 0) ? 0 : this;

	inline function new(v) this = v;

	static public inline function by(amount:Int)
		return new Progress(amount);

	@:to inline function toBool():Bool
		return this > 0;

	static public inline var EOF:Progress = new Progress(-1);
	static public inline var NONE:Progress = new Progress(0);
}

class Issue4977 extends Test {
	function test() {
		eq(-1, Progress.EOF);
		eq(0, Progress.NONE);
	}
}