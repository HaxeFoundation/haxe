package unit.issues;

private abstract class Robot<T> {
	public function new() {}

	public function performTask(listener:T) {}

	public function toString() {
		return "Robot";
	}
}

@:functionalInterface
private interface IGreetRobot {
	function greet<T>(robot:Robot<T>):Void;
}

@:functionalInterface
private interface IMathOperation {
	function operate(a:Int, b:Int):Int;
}

private class MathRobot extends Robot<IMathOperation> {
	override function performTask(listener:IMathOperation) {
		super.performTask(listener);
		var result = listener.operate(3, 4);
	}
}

private class GreetRobot extends Robot<IGreetRobot> {
	var target:Robot<Dynamic>;

	public function new(target:Robot<Dynamic>) {
		super();
		this.target = target;
	}

	override function performTask(listener:IGreetRobot) {
		super.performTask(listener);
		listener.greet(target);
	}
}

class Issue11054 extends Test {
	#if jvm
	function test() {
		var robot1 = new MathRobot();
		var robot2 = new GreetRobot(robot1);

		robot1.performTask(add);
		robot1.performTask(function(a:Int, b:Int):Int {
			return a - b;
		});

		var called = false;
		robot2.performTask(function(target:Robot<Dynamic>) {
			called = true;
		});
		t(called);
	}
	#end

	static function add(a:Int, b:Int):Int {
		return a + b;
	}
}
