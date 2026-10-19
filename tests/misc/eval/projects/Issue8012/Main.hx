import haxe.ds.Vector;

abstract WriteOnly(Array<Int>) {
	public var length(get, never):Int;

	function get_length()
		return this.length;

	@:arrayAccess function set(i:Int, v:Int):Int
		return this[i] = v;
}

abstract FloatLength(Array<Int>) {
	public var length(get, never):Float;

	function get_length():Float
		return this.length;

	@:arrayAccess function get(i:Int):Int
		return this[i];
}

abstract NoLength(Array<Int>) {
	@:arrayAccess function get(i:Int):Int
		return this[i];
}

abstract StringKey(Array<Int>) {
	public var length(get, never):Int;

	function get_length()
		return this.length;

	@:arrayAccess function get(k:String):Int
		return 0;
}

abstract PrivateLength(Array<Int>) {
	var length(get, never):Int;

	function get_length()
		return this.length;

	@:arrayAccess function get(i:Int):Int
		return this[i];
}

class Main {
	static final EMPTY = new Vector<Int>(0);

	static function main() {
		switch (null : WriteOnly) {
			case [a]:
			case _:
		}
		switch (null : FloatLength) {
			case [a]:
			case _:
		}
		switch (null : NoLength) {
			case [a]:
			case _:
		}
		switch (null : StringKey) {
			case [a]:
			case _:
		}
		switch (null : PrivateLength) {
			case [a]:
			case _:
		}
		var v = Vector.fromArrayCopy([1]);
		switch v {
			case [a]:
			case EMPTY:
			case _:
		}
		var x = switch v {
			case [a]: a;
		}
	}
}
