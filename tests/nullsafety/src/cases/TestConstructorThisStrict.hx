package cases;

import Validator.shouldFail;

/**
 * Tests for using `this` in constructors with uninitialized fields.
 * Both Loose and Strict modes now behave the same way: error on `this` before fields are initialized.
 * Related to issue: https://github.com/HaxeFoundation/haxe/issues/12572
 * 
 * This file tests the null-safety behavior with various @:nullSafety annotations.
 */

// Helper class to receive `this` references
class UtilityForCStrict {
	public function new(c:Dynamic) {}
}

/**
 * Test @:nullSafety(Off) on the constructor
 * Problem 2: Adding @:nullSafety(Off) to the constructor should disable the check
 */
class TestConstructorThisStrict_ConstructorOff {
	final utility1:UtilityForCStrict;
	final utility2:UtilityForCStrict;

	@:nullSafety(Off)
	public function new() {
		// With @:nullSafety(Off) on constructor, this should pass
		utility1 = new UtilityForCStrict(this);
		utility2 = new UtilityForCStrict(this);
	}
}

/**
 * Test @:nullSafety(Off) on the class itself
 */
@:nullSafety(Off)
class TestConstructorThisStrict_ClassOff {
	final utility1:UtilityForCStrict;
	final utility2:UtilityForCStrict;

	public function new() {
		// With @:nullSafety(Off) on the class, this should pass
		utility1 = new UtilityForCStrict(this);
		utility2 = new UtilityForCStrict(this);
	}
}

/**
 * Problem 3: Using `this` before a field is initialized SHOULD fail (in both Loose and Strict)
 */
class TestConstructorThisStrict_BeforeSingleInit {
	final utility1:UtilityForCStrict;

	public function new() {
		// Using this before the field is initialized should fail
		utility1 = shouldFail(new UtilityForCStrict(this));
	}
}

/**
 * Problem 3 variant: Multiple fields, using this before all are initialized
 */
class TestConstructorThisStrict_BeforeAllInit {
	final utility1:UtilityForCStrict;
	final utility2:UtilityForCStrict;

	public function new() {
		// Using this before all fields are initialized should fail
		utility1 = shouldFail(new UtilityForCStrict(this));
		utility2 = shouldFail(new UtilityForCStrict(this));
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
 * Test that after all fields are initialized, `this` can be used
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
