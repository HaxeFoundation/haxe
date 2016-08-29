package unit.issues;

private class C {
	public var hash:Array<Int>;
	public function new() {
		hash = [1, 2, 3];
	}

	public function add(x) {
		hash[0] = x;
		return x;
	}
}

private class D {
	public var field:{value1: Int, value2: Int};
	public function new() {
		field = { value1: 1, value2: 2 };
	}

	public function set(x) {
		field.value1 = x;
		return x;
	}
}

private class StupidStringBuf {
	public var b:String;

	public inline function new() {
		b = "";
	}

	public inline function add(s:String) {
		b += s;
	}
}

class Issue5477 extends unit.Test  {
	static var idCounter = 0;

	function testArray() {
		idCounter = 0;

		var c = new C();
		almostId(c).hash[0] = c.add(4);
		eq(4, c.hash[0]);
		eq(1, idCounter);

		var c = new C();
		c.hash[0] += c.add(4);
		eq(5, c.hash[0]);

		var c = new C();
		c.hash[1] = almostId(c).hash[0] += {
			var x = c.add(4);
			x;
		}
		eq(5, c.hash[0]);
		eq(5, c.hash[1]);
		eq(2, idCounter);

		var c = new C();
		almostId(c).hash[1] += ({eq(3, idCounter); almostId(c);}).hash[0] += {
			var x = c.add(4);
			x;
		}
		eq(5, c.hash[0]);
		eq(7, c.hash[1]);
		eq(4, idCounter);

		var c = new C();
		c.hash[1] += c.hash[0] = {
			var x = c.add(4);
			x;
		}
		eq(4, c.hash[0]);
		eq(6, c.hash[1]);

		var c = new C();
		var tmp = c.add(4);
		c.hash[0] += tmp;
		eq(8, c.hash[0]);
	}

	function testField() {
		idCounter = 0;

		var d = new D();
		almostId(d).field.value1 = d.set(4);
		eq(4, d.field.value1);
		eq(1, idCounter);

		var d = new D();
		d.field.value1 += d.set(4);
		eq(5, d.field.value1);

		var d = new D();
		d.field.value2 = almostId(d).field.value1 += {
			var x = d.set(4);
			x;
		}
		eq(5, d.field.value1);
		eq(5, d.field.value2);
		eq(2, idCounter);

		var d = new D();
		almostId(d).field.value2 += ({eq(3, idCounter); almostId(d);}).field.value1 += {
			var x = d.set(4);
			x;
		}
		eq(5, d.field.value1);
		eq(7, d.field.value2);
		eq(4, idCounter);

		var d = new D();
		d.field.value2 += d.field.value2 = {
			var x = d.set(4);
			x;
		}
		eq(4, d.field.value1);
		eq(6, d.field.value2);

		var d = new D();
		var tmp = d.set(4);
		d.field.value1 += tmp;
		eq(8, d.field.value1);
	}

	function testStringBuf() {
		var buf = new StupidStringBuf();
		buf.add("1");
		buf.add(messUp(buf));
		eq("23", buf.b);

		var buf = new StupidStringBuf();
		buf.add("1");
		buf.b += messUp(buf);
		eq("13", buf.b);
	}

	function test() {
        var a = g();
        var b = ff() + a;
        eq(3, b);
    }

    static var x = 1;

    static function ff() {
        return x = 2;
    }

    static function g() {
        return x;
    }

	static function messUp(buf:StupidStringBuf) {
		buf.b = "2";
		return "3";
	}

	static function almostId<T>(c:T) {
		++idCounter;
		return c;
	}
}