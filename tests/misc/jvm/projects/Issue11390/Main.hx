package;

import test.Robot;
import test.RobotFactory;

class Main {
	public static function main() {
		var robot1 = RobotFactory.buildMathRobot();
		var robot2 = RobotFactory.buildGreetRobot(robot1);
		var robot3 = RobotFactory.buildManufactureRobot();

		robot1.performTask(add);
		robot1.performTask(function(a:Int, b:Int):Int {
			return a - b;
		});

		robot2.performTask(function (target:Robot) {
            trace('Hello, ${target.toString()}!');
        }, () -> {
			trace('Cleanup...');
		});

		robot3.performTask(function (robotType:String) {
			trace('Manufacturing ${robotType}...');
			return robot2;
		});
	}

	static function add(a:Int, b:Int):Int {
		return a + b;
	}
}