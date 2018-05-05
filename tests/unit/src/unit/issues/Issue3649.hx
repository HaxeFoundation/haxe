package unit.issues;

enum
abstract HttpStatus(Int) {
	var NotFound = 404;
	var MethodNotAllowed = 405;
}

class Issue3649 extends Test {

	function test() {
		eq("Not found", print(NotFound));
		eq("Method not allowed", print(MethodNotAllowed));
		eq("Unknown", print(cast 12));
	}

	static function print(status:HttpStatus) {
		return switch(status) {
			case NotFound:
				'Not found';
			case MethodNotAllowed:
				'Method not allowed';
			default:
				'Unknown';
		}
	}
}