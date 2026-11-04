package unit;

import haxe.Serializer;
import haxe.Unserializer;
import haxe.ds.List;

private enum Issue11864Enum {
	B;
	C(e1:Issue11864Enum, e2:Issue11864Enum);
}

class TestSerialize extends Test {
	function id<T>(v:T):T {
		return Unserializer.run(Serializer.run(v));
	}

	function test() {
		// basic types
		var values:Array<Dynamic> = [
			null,
			true,
			false,
			0,
			1,
			1506,
			-0xABCDEF,
			12.3,
			-1e10,
			"hello",
			"éé",
			"\r\n",
			"\n",
			"   ",
			"",
		];
		for (v in values)
			eq(id(v), v);

		t(Math.isNaN(id(Math.NaN)));
		t(id(Math.POSITIVE_INFINITY) > 0);
		f(id(Math.NEGATIVE_INFINITY) > 0);
		f(Math.isFinite(id(Math.POSITIVE_INFINITY)));
		f(Math.isFinite(id(Math.NEGATIVE_INFINITY)));

		// array/list
		doTestCollection([]);
		doTestCollection([1, 2, 4, 5]);
		doTestCollection([1, 2, null, null, null, null, null, 4, 5]);

		// date
		var d = Date.now();
		var d2 = id(d);
		t((d2 is Date));
		eq(d2.toString(), d.toString());
		
		// int64
		var small = haxe.Int64.make(0,1);
		var big1 = haxe.Int64.make(1,-1);
		var big2 = haxe.Int64.make(-1,0x7FFFFFFF);
		t(small == id(small));
		t(big1 == id(big1));
		t(big2 == id(big2));

		// object
		var o = {x: "a", y: -1.56, z: "hello"};
		var o2 = id(o);
		eq(o.x, o2.x);
		eq(o.y, o2.y);
		eq(o.z, o2.z);

		// class instance
		var c = new MyClass(999);
		c.intValue = 33;
		c.stringValue = "Hello";
		var c2 = id(c);
		t((c2 is MyClass));
		f(c == c2);
		eq(c2.intValue, c.intValue);
		eq(c2.stringValue, c.stringValue);
		eq(c2.get(), 999);
		// Class value
		eq(id(MyClass), MyClass);

		// enums
		Serializer.USE_ENUM_INDEX = false;
		doTestEnums();
		Serializer.USE_ENUM_INDEX = true;
		doTestEnums();
		// Enum value
		eq(id(MyEnum), MyEnum);

		// StringMap
		var h = new haxe.ds.StringMap();
		h.set("keya", 2);
		h.set("kéyb", -465);
		var h2 = id(h);
		t((h2 is haxe.ds.StringMap));
		eq(h2.get("keya"), 2);
		eq(h2.get("kéyb"), -465);
		eq(Lambda.count(h2), 2);

		// IntMap
		var h = new haxe.ds.IntMap();
		h.set(55, 2);
		h.set(-101, -465);
		var h2 = id(h);
		t((h2 is haxe.ds.IntMap));
		eq(h2.get(55), 2);
		eq(h2.get(-101), -465);
		eq(Lambda.count(h2), 2);

		// ObjectMap
		var h = new haxe.ds.ObjectMap();
		var a = new unit.MyAbstract.ClassWithoutHashCode(9);
		var b = new unit.MyAbstract.ClassWithoutHashCode(8);
		h.set(a, b);
		h.set(b, a);
		var h2 = id(h);
		t((h2 is haxe.ds.ObjectMap));
		// these are NOT the same objects
		f(h2.exists(a));
		f(h2.exists(b));
		// all these should still work
		t(h.exists(a));
		t(h.exists(b));
		eq(h.get(a), b);
		eq(h.get(b), a);
		var nothing = true;
		for (k in h2.keys()) {
			nothing = false;
			t(k.i == 8 || k.i == 9);
			t(h2.exists(k));
			var v = h2.get(k);
			t(v.i == 8 || v.i == 9);
		}
		f(nothing);

		// bytes
		doTestBytes(haxe.io.Bytes.alloc(0));
		doTestBytes(haxe.io.Bytes.ofString("A"));
		doTestBytes(haxe.io.Bytes.ofString("AB"));
		doTestBytes(haxe.io.Bytes.ofString("ABC"));
		doTestBytes(haxe.io.Bytes.ofString("ABCD"));
		doTestBytes(haxe.io.Bytes.ofString("héllé"));
		var b = haxe.io.Bytes.alloc(100);
		for (i in 0...b.length)
			b.set(i, i % 10);
		doTestBytes(b);
		doTestBytesCrossPlatform();
		doTestReset();

		// recursivity
		c.ref = c;
		Serializer.USE_CACHE = true;
		var c2 = id(c);
		Serializer.USE_CACHE = false;
		eq(c2.ref, c2);

		// errors
		#if !cpp
		exc(function() Unserializer.run(null));
		#end

		exc(function() Unserializer.run(""));
	}

	function testEnumRef() {
		var old = Serializer.USE_CACHE;
		Serializer.USE_CACHE = true;
		var e = C(B, B);
		var e2 = id(e);
		t(Type.enumEq(e, e2));
		Serializer.USE_CACHE = old;
	}

	function doTestEnums() {
		eq(id(MyEnum.A), MyEnum.A);
		eq(id(MyEnum.B), MyEnum.B);
		var c = MyEnum.C(0, "hello");
		t(Type.enumEq(id(c), c));
		t(Type.enumEq(id(MyEnum.D(MyEnum.D(c))), MyEnum.D(MyEnum.D(c))));
		t((id(c) is MyEnum));
		t(switch (id(c)) {
			case C(_, _): true;
			default: false;
		});

		eq(id(SimpleEnum.SE_A), SimpleEnum.SE_A);
		eq(id(SimpleEnum.SE_B), SimpleEnum.SE_B);
		eq(id(SimpleEnum.SE_C), SimpleEnum.SE_C);
		eq(id(SimpleEnum.SE_D), SimpleEnum.SE_D);
		t(id(SimpleEnum.SE_A) == SimpleEnum.SE_A);
	}

	function doTestCollection(a:Array<Dynamic>) {
		var a2 = id(a);
		eq(a2.length, a.length);
		for (i in 0...a.length)
			eq(a2[i], a[i]);
		var l = Lambda.list(a);
		var l2 = id(l);
		t((l2 is List));
		eq(l2.length, l.length);
		var it = l.iterator();
		for (x in l2)
			eq(x, it.next());
		f(it.hasNext());
	}

	function doTestBytes(b:haxe.io.Bytes) {
		var b2 = id(b);
		t((b2 is haxe.io.Bytes));
		eq(b2.length, b.length);
		for (i in 0...b.length)
			eq(b2.get(i), b.get(i));
	}

	function doTestBytesCrossPlatform() {
		var sample = 's340:AAECAwQFBgcICQoLDA0ODxAREhMUFRYXGBkaGxwdHh8gISIjJCUmJygpKissLS4vMDEyMzQ1Njc4OTo7PD0%P0BBQkNERUZHSElKS0xNTk9QUVJTVFVWV1hZWltcXV5fYGFiY2RlZmdoaWprbG1ub3BxcnN0dXZ3eHl6e3x9fn%AgYKDhIWGh4iJiouMjY6PkJGSk5SVlpeYmZqbnJ2en6ChoqOkpaanqKmqq6ytrq%wsbKztLW2t7i5uru8vb6:wMHCw8TFxsfIycrLzM3Oz9DR0tPU1dbX2Nna29zd3t:g4eLj5OXm5%jp6uvs7e7v8PHy8:T19vf4%fr7:P3%';

		// serialization
		var b = haxe.io.Bytes.alloc(255);
		for (i in 0...255)
			b.set(i, i);
		eq(sample, Serializer.run(b));

		// de-serialization
		var b:haxe.io.Bytes = Unserializer.run(sample);
		eq(255, b.length);
		for (i in 0...b.length) {
			var byte = b.get(i);
			eq(i, byte);
			if (i != byte)
				break;
		}
	}

	function doTestReset()
	{
		var serializer = new Serializer();
		serializer.useCache = true;

		// first serialization
		var obj1 = { a: 1, b: 2, c: "test" };
		serializer.serialize(obj1);
		var serialized1 = serializer.toString();
		var deserialized1 = Unserializer.run(serialized1);

		eq(deserialized1.a, obj1.a);
		eq(deserialized1.b, obj1.b);
		eq(deserialized1.c, obj1.c);

		// reset the serializer
		serializer.reset();

		// checks if buffer is empty after reset
		eq(serializer.toString(), "");

		// we serialize a second, different object
		var obj2 = { x: 42, y: "new", z: false };
		serializer.serialize(obj2);
		var serialized2 = serializer.toString();
		var deserialized2 = Unserializer.run(serialized2);

		eq(deserialized2.x, obj2.x);
		eq(deserialized2.y, obj2.y);
		eq(deserialized2.z, obj2.z);

		// ensures serialization changed after reset
		f(serialized1 == serialized2);

		// resets again and serialize obj2 again to verify consistency
		serializer.reset();
		serializer.serialize(obj2);
		var serialized2Again = serializer.toString();

		// serialization should be identical if reset() truly resets state
		eq(serialized2, serialized2Again);

		// check against a fresh serializer instance
		var freshSerializer = new Serializer();
		freshSerializer.useCache = true;
		freshSerializer.serialize(obj2);
		var freshSerialized2 = freshSerializer.toString();

		eq(serialized2, freshSerialized2);
	}
}
