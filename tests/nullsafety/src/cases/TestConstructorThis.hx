package cases;

import Validator.shouldFail;

/**
 * Tests for using `this` in constructors with uninitialized fields.
 * Related to issue: https://github.com/HaxeFoundation/haxe/issues/xxxxx
 */

// Helper class to receive `this` references
class UtilityForC {
	public function new(c:Dynamic) {}
}

/**
 * Problem 1: In Loose mode, using `this` before all instance fields 
 * are initialized should NOT fail.
 * 
 * This test file is compiled with Loose mode via test.hxml
 */
class TestConstructorThis_LooseMode_Multi {
	final utility1:UtilityForC;
	final utility2:UtilityForC;

	public function new() {
		// In Loose mode, this should pass without errors
		utility1 = new UtilityForC(this);
		utility2 = new UtilityForC(this);
	}
}

/**
 * Problem 1 variant: Single field case in Loose mode
 */
class TestConstructorThis_LooseMode_Single {
	final utility1:UtilityForC;

	public function new() {
		// In Loose mode, this should pass without errors
		utility1 = new UtilityForC(this);
	}
}
