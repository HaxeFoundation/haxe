class Main {
	static function main() {
		var v = 1;
		var success = false;
		function next(b:Bool) {
			run(v, bool(), function() {
				if(b) next(!b)
				else success = true;
			});
		}
		next(true);
		if(!success) {
			throw 'Test failed';
		}
	}

	static function bool():Bool {
		return true;
	}

	static function run(v:Int, b:Bool, cb:()->Void) {
		cb();
	}
}