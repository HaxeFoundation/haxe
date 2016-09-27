package unit.issues;
import unit.Test;

class Issue4089 extends Test {
	static var timestamp = 1.0;

	function test() {
		function printTime(time:Float) {
			return time;
		}
		var p1 = printTime.bind(timestamp);
		feq(1.0, p1());
		timestamp = 2.0;
		feq(1.0, p1());
	}
}