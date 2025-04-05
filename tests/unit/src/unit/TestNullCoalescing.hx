package unit;

private class A {}
private class B extends A {}
private class C extends A {}

private class NullCoalClass {
	@:isVar public var field(get, set):String;

	public var getCounter = 0;
	public var setCounter = 0;

	public function new() {}

	public function get_field() {
		getCounter++;
		return field;
	}

	public function set_field(v:String) {
		setCounter++;
		return field = v;
	}
}

private typedef NullCoalAbstractData = {
	var field:String;
	var getCounter:Int;
	var setCounter:Int;
}

private abstract NullCoalAbstract(NullCoalAbstractData) {
	public var field(get, set):String;

	public function new() {
		this = {
			field: null,
			getCounter: 0,
			setCounter: 0
		}
	}

	public function getGetCounter() {
		return this.getCounter;
	}

	public function getSetCounter() {
		return this.setCounter;
	}

	public function get_field() {
		this.getCounter++;
		return this.field;
	}

	public function set_field(v:String) {
		this.setCounter++;
		return this.field = v;
	}
}

private abstract NullCoalAbstractResolve(NullCoalAbstractData) {
	public function new() {
		this = {
			field: null,
			getCounter: 0,
			setCounter: 0
		}
	}

	public function getGetCounter() {
		return this.getCounter;
	}

	public function getSetCounter() {
		return this.setCounter;
	}

	@:op(a.b) public function readResolve(field:String) {
		this.getCounter++;
		return Reflect.field(this, field);
	}

	@:op(a.b) public function writeResolve<T>(field:String, v:T) {
		this.setCounter++;
		Reflect.setField(this, field, v);
		return Reflect.field(this, field);
	}
}

@:nullSafety(StrictThreaded)
class TestNullCoalescing extends Test {
	final nullInt:Null<Int> = null;
	final nullFloat:Null<Float> = null;
	final nullBool:Null<Bool> = null;
	final nullString:Null<String> = null;

	var count = 0;

	function call() {
		count++;
		return "_";
	}

	function test() {
		count = 0;
		eq(true, 0 != 1 ?? 2);
		var a = call() ?? "default";
		eq(count, 1);

		eq(nullInt ?? nullInt, null);
		eq(nullBool ?? nullBool, null);

		final a:Dynamic = Std.random(0) + 1;
		final b = Std.random(0) + 2;
		eq(1 + a + 1 ?? 1 + b + 1, 3);

		final nullableBool:Null<Bool> = false;
		final testBool = nullBool ?? true;
		final testNullBool = nullBool ?? nullableBool;
		final s:Int = nullInt == null ? 2 : nullInt;
		final s:Int = if (nullInt == null) 2; else nullInt;
		final s = nullInt ?? 2;

		f(HelperMacros.isNullable(testBool));
		t(HelperMacros.isNullable(testNullBool));
		f(HelperMacros.isNullable(s));
		// nullsafety filter test:
		final shouldBeBool:Bool = testBool;
		if (testNullBool == null) {}
		final shouldBeInt:Int = s;

		eq(testBool, true);
		eq(testNullBool, false);
		eq(s, 2);

		eq(nullInt == null ? 2 : nullInt, 2);
		eq(nullInt ?? 2, 2);
		eq(nullInt ?? (2 : Null<Int>) ?? 3 + 100, 2);
		eq(nullInt ?? nullInt ?? 3, 3);
		f(HelperMacros.isNullable(nullInt ?? nullInt ?? 3));

		final i:Null<Int> = 1;
		final arr:Array<Int> = [i ?? 2];
		arr.push(i ?? 2);
		arr.push((1 : Null<Int>) ?? 2);
		eq(arr[0], 1);
		eq(arr[1], 1);
		eq(arr[2], 1);

		final arr = [nullInt ?? 2, 2];
		eq(arr[0], arr[1]);

		var a = [0 => nullInt ?? 0 + 100];
		eq(a[0], 100);

		final di:Null<Dynamic> = null;
		final di2:Null<Dynamic> = null;
		final di3:Null<Dynamic> = 2;
		eq(di ?? di2 ?? di3, 2);

		final a:Null<Int> = ({} : Dynamic).x;
		eq(a ?? 2, 2);

		final a = nullInt;
		eq(a ?? 2, 2);

		final a = nullString;
		eq(a ?? "2", "2");

		eq(1 ?? 2, 1);
		eq("1" ?? "2", "1");

		final arr = [];
		function item(n) {
			arr.push(n);
			return n;
		}
		eq(item(1) ?? item(2) ?? item(3), 1);
		eq(arr.length, 1);
		for (i => v in [1])
			eq(arr[i], v);

		final arr = [];
		function item(n) {
			arr.push(n);
			return null;
		}
		eq(item(1) ?? item(2) ?? item(3), null);
		eq(arr.length, 3);
		for (i => v in [1, 2, 3])
			eq(arr[i], v);

		var b:B = cast null;
		var c:C = cast null;
		var a = if (b != null) b else c;
		var a = b ?? c;
		eq("unit._TestNullCoalescing.A", HelperMacros.typeString(a));

		var nullF = false ? nullFloat : 0;
		var nullF2 = nullFloat ?? nullInt;
		var notNullF = nullFloat ?? 0;
		var notNullF2 = (1 : Null<Float>) ?? throw "";
		t(HelperMacros.isNullable(nullF));
		t(HelperMacros.isNullable(nullF2));
		f(HelperMacros.isNullable(notNullF));
		f(HelperMacros.isNullable(notNullF2));
	}

	function testAssignOp() {
		count = 0;
		var a:Null<Int> = null;
		a ??= 5;
		eq(a, 5);
		t(HelperMacros.isNullable(a ??= null));
		f(HelperMacros.isNullable(a ??= 5));
		var a:Null<Int> = null;
		eq(a ??= 5, 5);
		eq(a, 5);
		var a = "default";
		eq(a ??= "5", "default");

		count = 0;
		var a = call();
		eq(count, 1);
		a ??= call();
		eq(count, 1);

		var a:Null<String> = null;
		final b = a ??= call();
		final c = a ??= call();
		eq(count, 2);
		eq(a, "_");
		eq(b, "_");
		eq(c, "_");

		final map:Map<String, Array<Int>> = [];
		var array1 = [];
		map["foo"] ??= array1;
		eq(map["foo"], array1);
		map["foo"] ??= [];
		eq(map["foo"], array1);

		// test typing
		#if !macro
		var a = mut() ?? mut();
		eq(2, getMut());
		resetMut();

		var a:Null<Int> = 0;
		mutAssignLeft(a) ??= mut() ?? mut();
		eq(3, getMut());
		resetMut();

		var a:Null<Int> = 0;
		final b = a ??= mut();
		eq(1, getMut());
		resetMut();

		var a:Null<Int> = 0;
		mutAssignLeft(a) ??= 1;
		eq(1, getMut());
		resetMut();

		// field
		var obj = getObj();
		obj.field ??= "value";
		eq("value", obj.field ?? "fail");

		var value = obj.field ??= "value2";
		eq("value", obj.field ?? "fail");
		eq("value", value);

		mutAssignLeft(obj.field) ??= "not value";
		eq(1, getMut());
		eq("value", obj.field ?? "fail");
		resetMut();

		// accessor
		var obj = new NullCoalClass();
		obj.field ??= "value";
		eq(1, obj.getCounter);
		eq(1, obj.setCounter);
		eq("value", obj.field ?? "fail");

		var value = obj.field ??= "value2";
		eq(3, obj.getCounter);
		eq(1, obj.setCounter);
		eq("value", obj.field ?? "fail");
		eq("value", value);

		mutAssignLeft(obj.field) ??= "not value";
		eq(5, obj.getCounter);
		eq(1, obj.setCounter);
		eq(1, getMut());
		eq("value", obj.field ?? "fail");
		resetMut();

		// static extension accessor
		var obj = new NullCoalAbstract();
		obj.field ??= "value";
		eq(1, obj.getGetCounter());
		eq(1, obj.getSetCounter());
		eq("value", obj.field ?? "fail");

		var value = obj.field ??= "value2";
		eq(3, obj.getGetCounter());
		eq(1, obj.getSetCounter());
		eq("value", obj.field ?? "fail");
		eq("value", value);

		mutAssignLeft(obj.field) ??= "not value";
		eq(5, obj.getGetCounter());
		eq(1, obj.getSetCounter());
		eq(1, getMut());
		eq("value", obj.field ?? "fail");
		resetMut();

		// resolve
		var obj = new NullCoalAbstractResolve();
		obj.field ??= "value";
		eq(1, obj.getGetCounter());
		eq(1, obj.getSetCounter());
		eq("value", obj.field ?? "fail");

		var value = obj.field ??= "value2";
		eq(3, obj.getGetCounter());
		eq(1, obj.getSetCounter());
		eq("value", obj.field ?? "fail");
		eq("value", value);

		// TODO: this fails at the moment with some "not enough arguments error"
		// mutAssignLeft(obj.field) ??= "not value";
		// eq(5, obj.getGetCounter());
		// eq(1, obj.getSetCounter());
		// eq(1, getMut());
		// eq("value", obj.field ?? "fail");
		// resetMut();
		#end
	}

	static var mutI = 0;

	static function getObj<T>():{field:Null<T>} {
		return {field: null}
	}

	static macro function mut() {
		mutI++;
		return macro mutI;
	}

	static macro function getMut() {
		return macro $v{mutI};
	}

	static macro function resetMut() {
		mutI = 0;
		return macro $v{mutI};
	}

	static macro function mutAssignLeft(e:haxe.macro.Expr) {
		mutI++;
		return e;
	}
}
