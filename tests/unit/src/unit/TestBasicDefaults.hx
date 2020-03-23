package unit;

abstract W(Int) from Int to Int {}

class TestBasicDefaults extends Test {

	static var staticInt:Int;
	static var staticFloat:Float;
	static var staticBool:Bool;
	static var staticAbstract:W;

	var instanceInt:Int;
	var instanceFloat:Float;
	var instanceBool:Bool;
	var instanceAbstract:W;

	function test(){
		eq(0, staticInt);
		eq(0, staticFloat);
		eq(false, staticBool);
		eq(0, staticAbstract);

		eq(0, instanceInt);
		eq(0, instanceFloat);
		eq(false, instanceBool);
		eq(0, instanceAbstract);
	}
}
