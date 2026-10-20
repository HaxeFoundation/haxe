package unit.issues;

import haxe.ds.Vector;
import unit.HelperMacros.getErrorMessage;

private abstract Pair<T>(Array<T>) {
	public inline function new(a:T, b:T)
		this = [a, b];

	public var length(get, never):Int;

	inline function get_length()
		return this.length;

	@:arrayAccess inline function get(i:Int):T
		return this[i];
}

private abstract WriteOnly(Array<Int>) {
	public var length(get, never):Int;

	inline function get_length()
		return this.length;

	@:arrayAccess inline function set(i:Int, v:Int):Int
		return this[i] = v;
}

private abstract FloatLength(Array<Int>) {
	public var length(get, never):Float;

	inline function get_length():Float
		return this.length;

	@:arrayAccess inline function get(i:Int):Int
		return this[i];
}

private abstract NoLength(Array<Int>) {
	@:arrayAccess inline function get(i:Int):Int
		return this[i];
}

// Since #13071, `for` loops and array patterns index abstracts the way the typer does: these can now be iterated
// and matched, see testForwarded.
@:forward(length)
private abstract ForwardedLength(Array<Int>) {
	public inline function new(a:Array<Int>)
		this = a;

	@:arrayAccess inline function get(i:Int):Int
		return this[i];
}

@:forward(length) @:arrayAccess
private abstract ForwardedIndex(Array<String>) {
	public inline function new(a:Array<String>)
		this = a;
}

private abstract ToArray(Array<Int>) to Array<Int> {
	public inline function new(a:Array<Int>)
		this = a;

	public var length(get, never):Int;

	inline function get_length()
		return this.length;
}

@:forward(length)
private abstract ToArrayForwarded<T>(Array<T>) to Array<T> {
	public inline function new(a:Array<T>)
		this = a;
}

#if js
@:native("Array")
private extern class ArrayLike<T> implements ArrayAccess<T> {
	var length(default, null):Int;
	function new();
	function push(v:T):Int;
}

@:coreType @:arrayAccess
private abstract CoreArrayLike<T> {
	public var length(get, never):Int;

	inline function get_length():Int
		return (cast this : Array<T>).length;
}
#end

private enum Wrapper {
	Wrap(v:Vector<Int>);
}

class Issue8012 extends Test {
	static function vec(a:Array<Int>)
		return Vector.fromArrayCopy(a);

	static function describe(v:Vector<Int>) {
		return switch v {
			case []: "empty";
			case [x]: 'one $x';
			case [1, y]: 'one-then $y';
			case [x, y] if (x > y): 'desc $x $y';
			case [x, y]: 'two $x $y';
			case _: 'many ${v.length}';
		}
	}

	static function nullable(v:Null<Vector<Int>>) {
		return switch v {
			case null: "null";
			case [a, b]: 'pair $a $b';
			case _: "other";
		}
	}

	static function nullableNoNullCase(v:Null<Vector<Int>>) {
		return switch v {
			case [a, b]: 'pair $a $b';
			case _: "other";
		}
	}

	static function first<T>(v:Vector<T>):Null<T> {
		return switch v {
			case [a] | [a, _]: a;
			case _: null;
		}
	}

	function testVector() {
		eq("empty", describe(vec([])));
		eq("one 5", describe(vec([5])));
		eq("one-then 7", describe(vec([1, 7])));
		eq("desc 9 3", describe(vec([9, 3])));
		eq("two 3 9", describe(vec([3, 9])));
		eq("many 3", describe(vec([3, 9, 4])));
	}

	function testNull() {
		eq("null", nullable(null));
		eq("pair 1 2", nullable(vec([1, 2])));
		eq("other", nullable(vec([1])));
		eq("other", nullableNoNullCase(null));
		eq("pair 1 2", nullableNoNullCase(vec([1, 2])));
	}

	function testNested() {
		var nested = Vector.fromArrayCopy([vec([1]), vec([2, 3])]);
		eq(6, switch nested {
			case [[a], [b, c]]: a + b + c;
			case _: -1;
		});
		eq(20, switch Wrap(vec([4, 5])) {
			case Wrap([a, b]): a * b;
			case _: -1;
		});
	}

	function testOrAndExtractor() {
		eq("x", first(Vector.fromArrayCopy(["x"])));
		eq("x", first(Vector.fromArrayCopy(["x", "y"])));
		eq(null, first(Vector.fromArrayCopy(["x", "y", "z"])));
		eq("two", switch vec([2, 3]) {
			case _.length => 2: "two";
			case _: "other";
		});
		eq(5, switch vec([2, 3]) {
			case [_ * 2 => 4, b]: 2 + b;
			case _: -1;
		});
	}

	function testAbstract() {
		var p = new Pair("a", "b");
		eq("ab", switch p {
			case [a, b]: a + b;
			case _: "";
		});
		eq("", switch p {
			case [a]: a;
			case _: "";
		});
	}

	function testForwarded() {
		var a = new ForwardedLength([2, 3]);
		eq(6, switch a {
			case [x, y]: x * y;
			case _: 0;
		});
		var sum = 0;
		for (x in a)
			sum += x;
		eq(5, sum);
		var b = new ForwardedIndex(["a", "b"]);
		eq("b", switch b {
			case [_, s]: s;
			case _: "";
		});
		eq("ab", [for (s in b) s].join(""));
		var c = new ToArray([4]);
		eq(4, switch c {
			case [x]: x;
			case _: 0;
		});
		eq(4, [for (x in c) x][0]);
		var d = new ToArrayForwarded(["x", "y", "z"]);
		eq("z", switch d {
			case [_, _, s]: s;
			case _: "";
		});
		eq("xyz", [for (s in d) s].join(""));
	}

	#if js
	function testExtern() {
		var a = new ArrayLike<Int>();
		a.push(5);
		a.push(6);
		eq(30, switch a {
			case [a, b]: a * b;
			case _: 0;
		});
	}

	function testCoreType() {
		var a:CoreArrayLike<Int> = cast [7, 8];
		eq(56, switch a {
			case [a, b]: a * b;
			case _: 0;
		});
	}
	#end

	function testErrors() {
		eq("Unrecognized pattern: [a]", getErrorMessage(switch (null : WriteOnly) {
			case [a]:
			case _:
		}));
		eq("Unrecognized pattern: [a]", getErrorMessage(switch (null : FloatLength) {
			case [a]:
			case _:
		}));
		eq("Unrecognized pattern: [a]", getErrorMessage(switch (null : NoLength) {
			case [a]:
			case _:
		}));
		eq("Unrecognized pattern: [a]", getErrorMessage(switch ("foo") {
			case [a]:
			case _:
		}));
		eq("Unmatched patterns: _", getErrorMessage(switch vec([1]) {
			case [a]: a;
		}));
	}
}
