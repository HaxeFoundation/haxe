package unit.issues;

class Issue8912 extends Test {
#if !cppia // see https://github.com/HaxeFoundation/haxe/issues/8915
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
#end
}