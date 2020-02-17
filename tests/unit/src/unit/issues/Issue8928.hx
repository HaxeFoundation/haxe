package unit.issues;

class Issue8928 extends Test {
	function test() {
		var d:Null<Direction> = Forward;
		eq(10, d.getSpeed());
	}
}

@:using(unit.issues.Issue8928)
enum Direction {
	Forward;
	Backward;
}

class DirectionTools {
	static public function getSpeed(dir:Null<Direction>) {
		return switch dir {
			case Forward | null: 10;
			case Backward: -10;
		}
	}
}