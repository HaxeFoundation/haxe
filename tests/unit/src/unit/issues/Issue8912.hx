package unit.issues;

class Issue8912 extends Test {
	function test() {
		function loop() {
			while(true) {
				exit();
				continue;
			}
		}
		try {
			loop();
		} catch(e:String) {
			noAssert();
		}
	}

	static function exit() {
		throw 'exit';
	}
}