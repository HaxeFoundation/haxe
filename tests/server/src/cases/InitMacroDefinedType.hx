package cases;

class InitMacroDefinedType extends TestCase {
	static function init(body:String, field = "") {
		return 'package mac;

import haxe.macro.Context;

class Init {
	$field

	public static function run() {
		$body
	}

	static function define() {
		Context.defineType(macro class InitGen {
			public static function hello() return "init-gen";
		});
	}
}';
	}

	function setupInit() {
		vfs.putContent("mac/Init.hx", init("Context.onAfterInitMacros(define);"));
		vfs.putContent("Main.hx", "class Main { static function main() { trace(InitGen.hello()); } }");
		return ["-main", "Main", "--macro", "mac.Init.run()", "--interp"];
	}

	function testMacroStopsDefining(_) {
		var args = setupInit();
		runHaxe(args);
		assertSuccess();
		assertHasPrint("Main.hx:1: init-gen");
		vfs.putContent("mac/Init.hx", init(""));
		runHaxe(args);
		assertErrorMessage("Unknown identifier : InitGen");
	}

	function testMacroFails(_) {
		var args = setupInit();
		runHaxe(args);
		assertSuccess();
		vfs.putContent("mac/Init.hx", init('var x:Int = "init"; Context.onAfterInitMacros(define);'));
		runHaxe(args);
		assertErrorMessage("String should be Int");
		assertErrorMessage("Unknown identifier : InitGen");
	}

	function testMacroClassFails(_) {
		var args = setupInit();
		runHaxe(args);
		assertSuccess();
		vfs.putContent("mac/Init.hx", init("Context.onAfterInitMacros(define);", 'static var x:Int = "init";'));
		for (_ in 0...2) {
			runHaxe(args);
			assertErrorMessage("String should be Int");
			Assert.isFalse(hasErrorMessage("This API cannot be used in the interpreter run-time"));
		}
		vfs.putContent("mac/Init.hx", init("Context.onAfterInitMacros(define);"));
		runHaxe(args);
		assertSuccess();
		assertHasPrint("Main.hx:1: init-gen");
	}
}
