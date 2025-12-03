package unit;

class Box {
	var value:Dynamic;

	public function new(value:Dynamic) {
		this.value = value;
	}

	public function get<T>():T {
		return value;
	}
}

interface IFoo {}

class Foo implements IFoo {
	public function new() {}
}

class Bar {
	public var val:Int;
}

private class Numbers {
	public var ui8 : hl.UI8;
	public var ui16 : hl.UI16;
	public var i32 : Int;
	public var i64 : hl.I64;
	public var guid : hl.GUID;
	public function new() {
	}
	public function loadInt64( v : hl.I64 ) {
		ui8 = cast v;
		ui16 = cast v;
		i32 = cast v;
		i64 = cast v;
		guid = cast v;
	}
	public function loadInt( v : Int ) {
		ui8 = cast v;
		ui16 = cast v;
		i32 = cast v;
		i64 = cast v;
		guid = cast v;
	}
	public function loadUI16( v : hl.UI16 ) {
		ui8 = cast v;
		ui16 = cast v;
		i32 = cast v;
		i64 = cast v;
		guid = cast v;
	}
	public function loadUI8( v : hl.UI8 ) {
		ui8 = cast v;
		ui16 = cast v;
		i32 = cast v;
		i64 = cast v;
		guid = cast v;
	}
}

private typedef NumbersNull = {
	?nui8 : hl.UI8,
	?nui16 : hl.UI16,
	?ni32 : Int,
	?ni64 : hl.I64,
	?nguid : hl.GUID,
}

class TestHL extends Test {
	function testRetTypeTP() {
		var box = new Box(new Foo());
		try {
			var bar:Bar = box.get();
			trace(bar.val);
			assert("Expected cast failure");
		} catch(e: haxe.Exception) {
			eq(e.message, "Can't cast unit.Foo to unit.Bar");
		}
	}

	function testRetTypeTPIFace() {
		var foo = new Foo();
		var ifoo: IFoo = foo;
		var box = new Box(ifoo);
		var bar:Foo = box.get();
		// important: eq must not be used as casting the values to dynamic papers over some issues
		t(foo == bar);
	}

	//private function refTest(i:hl.types.Ref<Int>):Void
	//{
		//i *= 2;
	//}

	private function refTestAssign(i:hl.Ref<Int>):Void
	{
		i.set(2);
	}

	public function testRef()
	{
		var i = 10;
		refTestAssign(i);
		eq(i, 2);

		//var i = 10;
		//refTest(i);
		//eq(i, 20);
	}

	public function testNumberCompare() {
		var num = new Numbers(); // prevent @:analyzer(ignore)
		var v : hl.I64 = haxe.Int64.make(0xF, 1);
		num.loadInt64(v);
		f(num.i32 == v);
		f(v == num.i32);
		f(num.ui16 == v);
		f(v == num.ui16);
		f(num.ui8 == v);
		f(v == num.ui8);

		var v : Int = 0xF0000001;
		num.loadInt(v);
		t(num.i64 == v);
		t(v == num.i64);
		f(num.ui16 == v);
		f(v == num.ui16);
		f(num.ui8 == v);
		f(v == num.ui8);

		var v : hl.UI16 = 0xF001;
		num.loadUI16(v);
		t(num.i64 == v);
		t(v == num.i64);
		t(num.i32 == v);
		t(v == num.i32);
		f(num.ui8 == v);
		f(v == num.ui8);

		var v : hl.UI8 = 0xF1;
		num.loadUI8(v);
		t(num.i64 == v);
		t(v == num.i64);
		t(num.i32 == v);
		t(v == num.i32);
		t(num.ui16 == v);
		t(v == num.ui16);
	}

	public function testNumbersNull() {
		var num : NumbersNull = {};
		t(cast(num.nui8, hl.UI16) == 0);
		t(cast(num.nui8, Int) == 0);
		t(cast(num.nui8, hl.I64) == 0);
		t(cast(num.nui8, hl.GUID) == 0);

		t(cast(num.nui16, hl.UI8) == 0);
		t(cast(num.nui16, Int) == 0);
		t(cast(num.nui16, hl.I64) == 0);
		t(cast(num.nui16, hl.GUID) == 0);

		t(cast(num.ni32, hl.UI8) == 0);
		t(cast(num.ni32, hl.UI16) == 0);
		t(cast(num.ni32, hl.I64) == 0);
		t(cast(num.ni32, hl.GUID) == 0);

		t(cast(num.ni64, hl.UI8) == 0);
		t(cast(num.ni64, hl.UI16) == 0);
		t(cast(num.ni64, Int) == 0);
		t(cast(num.ni64, hl.GUID) == 0);

		t(cast(num.nguid, hl.UI8) == 0);
		t(cast(num.nguid, hl.UI16) == 0);
		t(cast(num.nguid, Int) == 0);
		t(cast(num.nguid, hl.I64) == 0);

		var v = new Numbers();
		v.loadInt64(0x123456789ABCDEF0i64);
		var num : NumbersNull = {
			nui8 : v.ui8,
			nui16 : v.ui16,
			ni32 : v.i32,
			ni64 : v.i64,
			nguid : v.guid,
		}
		t(num.nui8 == v.ui8);
		t(num.nui16 == v.ui16);
		t(num.ni32 == v.i32);
		t(num.ni64 == v.i64);
		t(num.nguid == v.guid);
	}

	public function testNumberArray() {
		var arr : Array<hl.GUID> = [];
		var i64 : hl.I64 = haxe.Int64.make(1, 1);
		var guid : hl.GUID = haxe.Int64.make(2, -1);
		arr.push(i64);
		arr.push(guid);
		arr.push(10);
		t(arr[0] == i64);
		t(arr[1] == guid);
		t(arr[2] == 10);

		var arr : Array<hl.I64> = [];
		var i64 : hl.I64 = haxe.Int64.make(1, 1);
		var guid : hl.GUID = haxe.Int64.make(2, -1);
		arr.push(i64);
		arr.push(guid);
		arr.push(10);
		t(arr[0] == i64);
		t(arr[1] == guid);
		t(arr[2] == 10);

		var arr : Array<hl.UI16> = [];
		var ui16 : hl.UI16 = 8;
		var i32 : Int = 32;
		var i32big : Int = 0xDEADBEEF;
		arr.push(ui16);
		arr.push(i32);
		arr.push(i32big);
		arr.push(10);
		t(arr[0] == ui16);
		t(arr[1] == i32);
		f(arr[2] == i32big);
		t(arr[2] == 0xBEEF);
		t(arr[3] == 10);

		var arr : Array<hl.UI8> = [];
		var ui8 : hl.UI16 = 0xF7;
		var ui16 : hl.UI16 = 8;
		var i32 : Int = 32;
		var i32big : Int = 0xDEADBEEF;
		arr.push(ui16);
		arr.push(i32);
		arr.push(i32big);
		arr.push(10);
		arr.push(ui8);
		t(arr[0] == ui16);
		t(arr[1] == i32);
		f(arr[2] == i32big);
		t(arr[2] == 0xEF);
		t(arr[3] == 10);
		t(arr[4] == ui8);
	}

	public function testNumberOp() {
		var a = new Numbers();
		a.loadInt64(0x123456789ABCDEF0i64);
		var b = new Numbers();
		b.loadInt64(0xF0123456789ABCDEi64);

		t(a.ui8 + b.ui8 == (206 : hl.UI8));
		t(a.ui8 + b.ui8 == 206);
		t(a.ui8 - b.ui8 == 18);
		t(a.ui8 * b.ui8 == 32);
		t(a.ui8 << 1 == 480); // Int
		t(a.ui8 >> 1 == 120);
		t(a.ui8 % b.ui8 == 18);
		t(a.ui8 & b.ui8 == 208);
		t(a.ui8 | b.ui8 == 254);
		t(a.ui8 ^ b.ui8 == 46);

		t(a.ui16 + b.ui16 == (39886 : hl.UI16));
		t(a.ui16 + b.ui16 == 39886);
		t(a.ui16 - b.ui16 == 8722);
		t(a.ui16 * b.ui16 == 37920);
		t(a.ui16 << 1 == 114144); // Int
		t(a.ui16 >> 1 == 28536);
		t(a.ui16 % b.ui16 == 8722);
		t(a.ui16 & b.ui16 == 40144);
		t(a.ui16 | b.ui16 == 65278);
		t(a.ui16 ^ b.ui16 == 25134);

		t(a.ui8 + a.ui16 == 57312);
		t(a.ui16 + a.ui8 == 57312);
		t(a.ui8 + a.i32 == 224); // UI8
		t(a.i32 + a.ui8 == 224);
		t(a.ui16 + a.i32 == 48608); // UI16
		t(a.i32 + a.ui16 == 48608);

		t(a.i64 + b.i64 == 163971058432973774i64);
		t(a.i64 - b.i64 == 2459565876494606866i64);
		t(a.i64 * b.i64 == -3777164426036014048i64);
		t(a.i64 << 1 == a.i64 * 2i64);
		t(a.i64 >> 1 == a.i64 / 2i64);
		t(a.i64 / b.i64 == -1i64); // hl.I64.div return I64
		t(a.i64 % b.i64 == a.i64 + b.i64);
		t(a.i64 & b.i64 == b.i64 & a.i64);
		t(a.i64 | b.i64 == -993476380043837698i64);
		t(a.i64 | b.i64 == b.i64 | a.i64);
		t(a.i64 ^ b.i64 == -2150923818520649170i64);
		t(a.i64 ^ b.i64 == b.i64 ^ a.i64);
	}

	private function numberFun( v : Numbers, ui8 : hl.UI8, ui16 : hl.UI16, i32 : Int, i64: hl.I64, guid : hl.GUID ) {
		t(v.ui8 == ui8);
		t(v.ui16 == ui16);
		t(v.i32 == i32);
		t(v.i64 == i64);
		t(v.guid == guid);
	}

	private function numberFunOpt( ?v : Numbers, ?ui8 : hl.UI8, ?ui16 : hl.UI16, ?i32 : Int, ?i64: hl.I64, ?guid : hl.GUID ) {
		t(v?.ui8 == ui8);
		t(v?.ui16 == ui16);
		t(v?.i32 == i32);
		t(v?.i64 == i64);
		t(v?.guid == guid);
	}

	private function numberFunDefault( v : Numbers, ui8 : hl.UI8 = 1, ui16 : hl.UI16 = 1, i32 : Int = 1, i64: hl.I64 = 1, guid : hl.GUID = 1 ) {
		t(v.ui8 == ui8);
		t(v.ui16 == ui16);
		t(v.i32 == i32);
		t(v.i64 == i64);
		t(v.guid == guid);
	}

	public function testNumberCall() {
		var v = new Numbers();
		v.loadInt64(0x123456789ABCDEF0i64);
		numberFun(v, v.ui8, v.ui16, v.i32, v.i64, v.guid);
		numberFunOpt(v, v.ui8, v.ui16, v.i32, v.i64, v.guid);
		numberFunOpt();
		var v = new Numbers();
		v.loadInt(1);
		numberFunDefault(v);
	}
}
