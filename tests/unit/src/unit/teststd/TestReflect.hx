package unit.teststd;

class TestReflect extends unit.Test {
	public function test() {
		// hasField
		var x = { a: 1, b: null };
		t(Reflect.hasField(x, "a"));
		t(Reflect.hasField(x, "b"));
		f(Reflect.hasField(x, "c"));

		// field
		eq(Reflect.field(x, "a"), 1);
		eq(Reflect.field(x, "b"), null);
		eq(Reflect.field(x, "c"), null);
		var c = new C2();
		eq(Reflect.field(c, "v"), "var");
		eq(Reflect.field(c, "prop"), "prop");
		eq(Reflect.field(c, "func")(), "foo");
		eq(Reflect.field(c, "propAcc"), "0");
		var n = null;
		eq(Reflect.field(n, n), null);
		eq(Reflect.field(1, "foo"), null);

		// setField
		Reflect.setField(x, "a", 2);
		eq(x.a, 2);
		Reflect.setField(x, "c", "foo");
		eq(Reflect.field(x, "c"), "foo");
		var c = new C2();
		Reflect.setField(c, "v", "bar");
		eq(c.v, "bar");
		//Reflect.setField(c, "v2", "bar2");
		//c.v2 == "bar";
		//Reflect.setField(c, "func2", function() return "x");
		//Reflect.field(c, "func2")() == "x";

		// getProperty
		var c = new C2();
		eq(Reflect.getProperty(c, "v"), "var");
		eq(Reflect.getProperty(c, "prop"), "prop");
		eq(Reflect.getProperty(c, "func")(), "foo");
		eq(Reflect.getProperty(c, "propAcc"), "1");
		eq(Reflect.getProperty(null, "a"), null);
		eq(Reflect.getProperty(null, null), null);

		// setProperty
		Reflect.setProperty(x, "a", 2);
		eq(x.a, 2);
		Reflect.setProperty(x, "c", "foo");
		eq(Reflect.field(x, "c"), "foo");
		var c = new C2();
		Reflect.setProperty(c, "v", "bar");
		eq(c.v, "bar");
		Reflect.setProperty(c, "propAcc", "abc");
		eq(Reflect.field(c, "propAcc"), "ABC");

		// fields
		var names = ["a", "b", "c"];
		for (name in Reflect.fields(x)) {
			names.remove(name);
		}
		eq(names.length, 0);

		// isFunction
		var c = new C2();
		t(Reflect.isFunction(function() return 1));
		f(Reflect.isFunction(1));
		f(Reflect.isFunction(null));
		t(Reflect.isFunction(Reflect.field(c, "func")));

		// deleteField
		t(Reflect.hasField(x, "c"));
		Reflect.deleteField(x, "c");
		f(Reflect.hasField(x, "c"));
		Reflect.deleteField(x, "c");
		f(Reflect.hasField(x, "c"));

		// copy
		var y = Reflect.copy(x);
		eq(Reflect.field(y, "a"), 2);
		eq(Reflect.field(y, "b"), null);
		eq(Reflect.field(y, "c"), null);
		eq(Reflect.copy(null), null);

		//compare
		t(Reflect.compare(1,2) < 0);
		t(Reflect.compare(2,1) > 0);
		eq(Reflect.compare(1,1), 0);
		t(Reflect.compare("abcd","e") < 0);
		eq(Reflect.compare("abcd","abcd"), 0);
		t(Reflect.compare("e","abcd") > 0);
		eq(Reflect.compare(null,null), 0);
		t(Reflect.compare("abcd",null) != 0);
		t(Reflect.compare(null, "abcd") != 0);
		var a = haxe.Int64.make(0x7FFFFFFF, 0xFFFFFFF8); // 9223372036854775800
		var b = haxe.Int64.make(0x80000000, 0x00000007); // -9223372036854775801
		t(Reflect.compare(a, b) > 0);
		var a = 2147483640;
		var b = -2147483641;
		t(Reflect.compare(a, b) > 0);

		// compareMethods
		var x = function(t) return 1;
		var y = function(t) return -1;
		var z = function(t) return 1;
		f(Reflect.compareMethods(x,y));
		f(Reflect.compareMethods(x,z));
		f(Reflect.compareMethods(y,z));
		t(Reflect.compareMethods(x,x));
		t(Reflect.compareMethods(y,y));
		t(Reflect.compareMethods(z,z));

		f(Reflect.compareMethods(x,null));
		f(Reflect.compareMethods(null,x));

		// compareMethods with closures
		var a = [1];
		var b = [2];
		var v : Dynamic = a.push;
		t(Reflect.compareMethods(a.push, a.push));
		f(Reflect.compareMethods(a.push, a.pop));
		f(Reflect.compareMethods(a.push, b.push));
		t(Reflect.compareMethods(a.push, v));
		f(Reflect.compareMethods(b.push, v));

		// isObject
		t(Reflect.isObject({}));
		t(Reflect.isObject({v:"f"}));
		t(Reflect.isObject(new C()));
		t(Reflect.isObject(new C2()));
		t(Reflect.isObject(new CChild()));
		t(Reflect.isObject(new EmptyClass()));
		t(Reflect.isObject(Type.createEmptyInstance(ReallyEmptyClass)));
		t(Reflect.isObject("foo"));
		t(Reflect.isObject(E));
		t(Reflect.isObject(C));

		f(Reflect.isObject(1));
		f(Reflect.isObject(1.1));
		f(Reflect.isObject(true));
		f(Reflect.isObject(EA));
		f(Reflect.isObject(EVMB()));
		f(Reflect.isObject(null));
		var x:C = null;
		f(Reflect.isObject(x));

		// isEnumValue
		t(Reflect.isEnumValue(EA));
		t(Reflect.isEnumValue(EVMB()));

		f(Reflect.isEnumValue({}));
		f(Reflect.isEnumValue({v:"f"}));
		f(Reflect.isEnumValue(new C()));
		f(Reflect.isEnumValue(new C2()));
		f(Reflect.isEnumValue(new CChild()));
		f(Reflect.isEnumValue(new EmptyClass()));
		f(Reflect.isEnumValue(Type.createEmptyInstance(ReallyEmptyClass)));
		f(Reflect.isEnumValue("foo"));
		f(Reflect.isEnumValue(E));
		f(Reflect.isEnumValue(C));
		f(Reflect.isEnumValue(1));
		f(Reflect.isEnumValue(1.1));
		f(Reflect.isEnumValue(true));
		f(Reflect.isEnumValue(null));
		var x:C = null;
		f(Reflect.isEnumValue(x));

	}
}
