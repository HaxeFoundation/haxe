package cases;

import Validator.shouldFail;

/**
 * Tests for using `this` in constructors with uninitialized fields in Strict mode.
 * Related to issue: https://github.com/HaxeFoundation/haxe/issues/xxxxx
 * 
 * This file is compiled with Strict mode via test.hxml
 */

// Helper class to receive `this` references
class UtilityForCStrict {
	public function new(c:Dynamic) {}
}

/**
 * Problem 2: Adding @:nullSafety(Off) to the constructor should disable the check
 */
class TestConstructorThisStrict_ConstructorOff {
	final utility1:UtilityForCStrict;
	final utility2:UtilityForCStrict;

	@:nullSafety(Off)
	public function new() {
		// With @:nullSafety(Off) on constructor, this should pass even in Strict mode
		utility1 = new UtilityForCStrict(this);
		utility2 = new UtilityForCStrict(this);
	}
}

/**
 * Problem 3: In Strict mode, using `this` before a field is initialized SHOULD fail
 */
@:build(Validator.checkFields())
class TestConstructorThisStrict_BeforeSingleInit {
	@:shouldFail final utility1:UtilityForCStrict;

	public function new() {
		// In Strict mode, this should fail because utility1 is not initialized yet
		shouldFail(utility1 = new UtilityForCStrict(this));
	}
}

/**
 * Problem 3 variant: Multiple fields, using this before all are initialized
 */
@:build(Validator.checkFields())
class TestConstructorThisStrict_BeforeAllInit {
	@:shouldFail final utility1:UtilityForCStrict;
	@:shouldFail final utility2:UtilityForCStrict;

	public function new() {
		// In Strict mode, this should fail because not all fields are initialized
		shouldFail(utility1 = new UtilityForCStrict(this));
		shouldFail(utility2 = new UtilityForCStrict(this));
	}
}

/**
 * Problem 4: Using @:nullSafety(Off) on a specific field assignment should work
 */
class TestConstructorThisStrict_AssignmentOff {
	final utility1:UtilityForCStrict;

	public function new() {
		// With @:nullSafety(Off) on the assignment, this should pass
		@:nullSafety(Off) utility1 = new UtilityForCStrict(this);
	}
}

/**
 * Test that after all fields are initialized, `this` can be used in Strict mode
 */
class TestConstructorThisStrict_AfterAllInit {
	final utility1:UtilityForCStrict;

	public function new() {
		utility1 = new UtilityForCStrict(null);
		// After all fields are initialized, this should work
		acceptThis(this);
	}

	static function acceptThis(t:TestConstructorThisStrict_AfterAllInit) {}
}
