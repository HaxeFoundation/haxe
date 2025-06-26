package unit;

abstract W(Int) from Int {
	@:to inline public function toString():String return '$this';
}

class TestArrowFunctions extends Test {

	var f0_0: () -> Int;
	var f0_1: () -> W;

	var f1_0: Int->Int;
	var f1_1: ?Int->Int;

	var f2_0: Int->Int;

	var f3_0: Int->Int->Int;
	var f3_1: ?Int->String->Int;
	var f3_2: Int->?Int->Int;

	var f4:   Int->(Int->Int);
	var f5:   Int->Int->(Int->Int);
	var f6_a: Int->(Int->(Int->Int));
	var f6_b: Int->(Int->(Int->Int));
	var f7:   (Int->Int)->(Int->Int);
	var f8:   Int -> String;
	var f9:   Float->Bool->String;
	var arr: Array<Int->Int> = [];
	var map: Map<Int,Int->Int> = new Map();
	var obj: { f : Int->Int };

	var v0:   Int;
	var v1:   String;

	var maybe : Void -> Bool;

	function testSyntax(){
		maybe = () -> Math.random() > 0.5;

		v0 = (123);
		v0 = (123:Int);

		f0_0 = function () return 1;
		f0_0 = () -> 1;

		f0_0 = (() -> 1);
		f0_0 = (() -> 1:()->Int);
		f0_0 = cast (() -> 1:()->Int);

		v0 = f0_0();

		f0_1 = function () : W return 1;
		v1 = f0_1();

		f0_1 = () -> (1:W);
		v1 = f0_1();

		f1_0 = function (a:Int) return a;
		f1_1 = function (?a:Int) return a;

		f1_0 = a -> a;
		v0 = f1_0(1);

		f1_1 = (?a) -> a;
		v0 = f1_1(1);

		f1_1 = (?a:Int) -> a;
		v0 = f1_1(1);

		f1_1 = (a:Int=1) -> a;
		v0 = f1_1();

		f1_1 = (?a:Int=1) -> a;
		v0 = f1_1();

		f1_1 = function (a=2) return a;
		eq(f1_1(),2);

		f1_1 = (a=2) -> a;
		eq(f1_1(),2);

		f3_0 = function (a:Int, b:Int) return a + b;
		f3_1 = function (?a:Int, b:String) return a + b.length;
		f3_2 = function (a:Int, ?b:Int) return a + b;

		f3_0 = (a:Int, b:Int)  -> a + b;
		f3_1 = (?a:Int, b:String) -> a + b.length;
		f3_2 = (a:Int, ?b:Int) -> a + b;

		#if !flash // Cannot skip not nullable argument
		f3_1 = function (a=1, b:String) return a + b.length;
		eq(f3_1("--"),3);

		f3_1 = function (?a:Int=1, b:String) return a + b.length;
		eq(f3_1("--"),3);

		f3_2 = function (a:Int, b=2) return a + b;
		eq(f3_2(1),3);

		f3_1 = (a=1, b:String) -> a + b.length;
		eq(f3_1("--"),3);

		f3_1 = (a:Int=1, b:String) -> a + b.length;
		eq(f3_1("--"),3);

		f3_1 = (?a:Int=1, b:String) -> a + b.length;
		eq(f3_1("--"),3);

		f3_2 = (a:Int, b=2) -> a + b;
		eq(f3_2(1),3);
		#end

		f4 = function (a) return function (b) return a + b;
		f4 = a -> b -> a + b;

		f5 = function (a,b) return function (c) return a + b + c;
		f5 = (a, b) -> c -> a + b + c;

		f6_a = function (a) return function (b) return function (c) return a + b + c;
		f6_b = a -> b -> c -> a + b + c;
		eq(f6_a(1)(2)(3),f6_b(1)(2)(3));

		f7 = function (f:Int->Int) return f;
		f7 = f -> f;
		f7 = (f:Int->Int) -> f;
		f7 = maybe() ? f -> f : f -> g -> f(g);
		f7 = switch maybe() {
			case true:  f -> f;
			case false: f -> g -> f(g);
		};

		f8 = (a:Int) -> ('$a':String);
		var _f9 = (a:Float,b:Bool) -> '$a $b';
		f9 = _f9;

		arr = [for (i in 0...5) a -> a * i];
		arr = [a -> a + a, b -> b + b, c -> c + c];
		arr.map( f -> f(2) );

		var arr2:Array<Int->W> = [for (f in arr) x -> f(x)];

		map = [1 => a -> a + a, 2 => a -> a + a, 3 => a -> a + a];

		obj = { f : a -> a + a };
	}
}
