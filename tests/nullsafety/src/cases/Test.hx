package cases;

import Validator.shouldFail;
import haxe.Unsafe;

private enum DummyEnum {
	DummyOne;
	DummyTwo(a:Int, ?b:String);
}

typedef TWrap<T> = T;

abstract AWrap<T>(T) from T to T {
	function abstracts_shouldBeChecked(?a:String) {
		shouldFail(var s:String = a);
	}
}

class Generic<T> {
	public var value:T;
	public function new(value:T) {
		this.value = value;
	}
}

typedef AnonAsClass = {
	@:optional var optional:String;
}

typedef AnonAsStruct = {
	?optional:String
}

/** Test `@:safety(false)` is respected on fields */
class UnsafeFields {
	@:safety(false) var unsafeVar:String = null;
	@:safety(false) var notInitializedField:String;

	@:safety(false)
	static function unsafeMethod() {
		var s:String = null;
	}

	static function unsafeExpr() {
		var s:String;
		(s = null:Unsafe<String>);
	}
}

/** Test `@:safety(false)` is respected on a class */
@:safety(false)
class UnsafeClass {
	var uninitializedVar:String;

	public function new() {
		doStuff(this); //pass this somewhere before all fields are initialized
	}

	function doStuff(t:UnsafeClass) {}
}

@:build(Validator.checkFields())
private class TestWithoutConstructor {
	@:shouldFail var notInitializedField:String;
}

@:build(Validator.checkFields())
class Test {
	public var field:Null<String>;
	// @:shouldWarn public var publiclyModifiableField:String = 'hello';
	@:shouldFail var notInitializedField:Int;
	@:shouldFail var notInitializedProperty(default,null):Float;
	@:shouldFail @:isVar var notInitializedIsVar(get,set):String;
	@:shouldFail var initializedWithNullable:String = null;
	function get_notInitializedIsVar() return notInitializedIsVar;
	function set_notInitializedIsVar(v) return notInitializedIsVar = v;
	var notReal(get,never):Int;
	function get_notReal() return 42;

	var initialized:Bool = false;
	var initializedInConstructor:String;
	var initializedInAllBranchesOfConstructor:String;
	@:shouldFail var initializedInSomeBranchesOfConstructor:String;

	/**
	 *  Null safety should work in __init__ functions
	 */
	static function __init__() {
		var s:Null<String> = 'hello';
		shouldFail(s.length);
	}

	static public function main() {
	}

	/**
	 *  Null safety should work in constructors
	 */
	public function new(a:String) {
		if(Std.random(2) == 0) {
			initializedInSomeBranchesOfConstructor = 'hello';
			initializedInAllBranchesOfConstructor = 'hello';
		} else {
			initializedInAllBranchesOfConstructor = 'hello';
		}
		var self = shouldFail(this);
		shouldFail(instanceMethod());
		var closure = shouldFail(instanceMethod);
		var notInitializedYet = shouldFail(initializedInConstructor);
		initializedInConstructor = 'hello';
		var s:Null<String> = 'hello';
		shouldFail(s.length);
	}

	function instanceMethod() {}

	static function fieldAccess_onNullableValue_shouldFail():Void {
		var a:Null<String> = "hello";
		shouldFail(a.length);
	}

	static function fieldAccess_onNullableValueInIfCondition_shouldFail():Void {
		var a:Null<String> = "hello";
		shouldFail(if(a.length == 0) {});
	}

	static function fieldAccess_onNotNullableValue_shouldPass():Void {
		var a:String = "hello";
		a.length;
	}

	static function fieldAccess_onOptionalNullableValue_shouldFail(a:String = null, ?b:String):Void {
		shouldFail(a.length);
		shouldFail(b.length);
	}

	static function fieldAccess_onOptionalNotNullableValue_shouldPass(a:String = 'hello'):Void {
		a.length;
	}

	static function call_onNullableValue_shouldFail() {
		var fn:Null<Void->Void> = function() {}
		shouldFail(fn());
	}

	static function call_onNotNullableValue_shouldPass() {
		var fn:Void->Void = function() {}
		fn();
	}

	static function call_nullableValueToNotNullableArgument_shouldFail() {
		var fn = function(a:String) {}
		var v:Null<String> = 'hello';
		shouldFail(fn(v));
	}

	static function call_nullableValueToOptionalArgument_shouldPass() {
		var fn = function(?a:Int) {}
		var v:Null<Int> = 1;
		fn(v);
	}

	static public function new_nullableValueToNotNullableArgument_shouldFail(?v:String) {
		shouldFail(new Test(v));
	}

	static public function new_nullableValueToNotNullableGenericArg_shouldFail(?n:String) {
		shouldFail(new Generic<String>(n));
	}

	static public function new_nullableValueToNullableGenericArg_shouldPass(?n:String) {
		new Generic(n);
	}

	static function varDecl_assignNullableValueToNotNullableVar_shouldFail() {
		var v:Null<String> = 'hello';
		shouldFail(var s:String = v);
		shouldFail(var s:String = null);
	}

	static function assign_nullableValueToNotNullable_shouldFail() {
		var a:Null<Int> = 0;
		var b = 10;
		shouldFail(b = a);
	}

	static function assign_notNullableValueToNullable_shouldPass() {
		var a:Null<Int> = null;
		var b = 10;
		a = b;
	}

	static function assign_safeValueToAnotherNullable_shouldInferSafety() {
		var s:Null<String> = null;
		var n;
		n = (s == null ? "hello" : s);
		var t:String = n;
	}

	static function binop_withNullableValue_shouldFail() {
		var a:Null<Int> = 0;
		var b = 10;
		shouldFail(a + b);
	}

	static function binop_comparisonWithNullableValue_shouldPass() {
		var a:Null<Int> = 0;
		var b = 10;
		a == b;
		a != b;
	}

	static function unop_nullableValue_shouldFail() {
		var a:Null<Int> = 0;
		shouldFail(a++);
	}

	static function ternary_nullableElse_assignToNotNullableValue_shouldFail() {
		var v:Null<String> = 'a';
		var a:String;
		shouldFail((true ? 'hello' : v).length);
	}

	static function arrayAccess_nullableArray_shouldFail() {
		var a:Null<Array<Int>> = [];
		shouldFail(a[0]);
	}

	static function arrayAccess_usingNullableIndex_shouldFail() {
		var a:Array<Int> = [0];
		var idx:Null<Int> = 0;
		shouldFail(a[idx]);
	}

	static function if_nullableCondition_shouldFail() {
		var s:Null<Bool> = false;
		shouldFail(if(s) {});
	}

	static function typeInference_arrayAccess_fieldOnNullableItem_shouldFail() {
		var a:Array<Null<String>> = [];
		shouldFail(a[0].length);
	}

	static function typeInference_assignNullableValueToVariableWithoutExplicitTyping_shouldPass(nullable:String = null) {
		var s = nullable;
	}

	static function typeInference_fieldAccessOnInferredNullableType_shouldFail(nullable:Null<String>) {
		var s = nullable;
		shouldFail(s.length);
	}

	static var notNullableSetter(default,set):String = 'hello';
	static function set_notNullableSetter(v) return notNullableSetter = v;
	static function setter_passNullableValueToNotNullableSetter_shouldFail(?v:String) {
		shouldFail(notNullableSetter = v);
	}

	static function checkAgainstNull_transferSafeNullableLocalToNotNullable_shouldPass(?a:String) {
		var s:String;
		if(a == null) {} else s = a;
		if(null == a) {} else s = a;
		if(a != null) s = a;
		if(null != a) s = a;
		s = (a == null ? 'hello' : a);
		s = (null == a ? 'hello' : a);
		s = (a != null ? a : 'hello');
		s = (null != a ? a : 'hello');
		s = if(a == null) {
			'hello';
		} else {
			'other expressions';
			a;
		}
	}

	static function checkAgainstNull_safeNullableLocalToNotNullableAfterModification(?a:String) {
		var s:String;
		if(a != null) {
			a = 'hello'; //not nullable expr
			s = a;
			if(Std.random(2) > 0) {
				var arr = [null, 'hello'];
				a = arr[0]; //nullable expr
			}
			shouldFail(s = a);
		}
	}

	static function checkAgainstNull_checkAndFieldAccess(?a:String) {
		var s:Null<String> = 'hello';
		if(s != null && s.length == 0) {}
		if(s == null || s.length == 0) {}
		s != null && s.length == 0;
		s == null || s.length == 0;

		shouldFail(if(s != null || s.length == 0) {});
		shouldFail(if(s == null && s.length == 0) {});
		shouldFail(s != null || s.length == 0);
		shouldFail(s == null && s.length == 0);

		//checked against not-nullable value, so it's not null
		var nullable:Null<String> = 'hello';
		var s = 'world';
		if(nullable == s) {
			s = nullable;
		} else {
			shouldFail(s = nullable);
		}
	}

	static function checkedAgainstNull_nullifiedAfterCheck_shouldFail(?a:String) {
		if(a != null) {
			a = null;
			shouldFail(var s:String = a);
		}
	}

	static function checkAgainstNull_checkOutsideLoop_shouldStaySafeInLoop(?a:String) {
		if(a != null) {
			for(i in 0...Std.random(10)) {
				var s:String = a;
			}
		}
	}

	static function checkAgainstNull_checkInLoop(?a:String) {
		var s:String;
		for(i in 0...Std.random(10)) {
			shouldFail(s = a);
			if(a != null) {
				s = a;
			}
			shouldFail(s = a);
		}
	}

	static function checkAgainstNull_checkOutsideLoopAndChangedToNullableInside_shouldBeUnsafeFromBeginningOfLoop(?a:String) {
		if(a != null) {
			for(i in 0...Std.random(10)) {
				shouldFail(var s:String = a);
				a = null;
			}
		}
	}

	static function checkedAgainstNull_checkedInClosure_shouldFail(?a:String) {
		function local() {
			if(a != null) {
				shouldFail(var s:String = a);
			}
		}
	}

	static function checkedAgainstNullAfterClosure_usedButNotModifiedInClosure_shouldPass(?a:String) {
		function local() {
			var c = a;
		}
		if(a != null) {
			var s:String = a;
		}
	}

	static function checkedAgainstNullAfterClosure_modifiedToNullableInClosure_shouldFail(?a:String) {
		function local() {
			a = null;
		}
		if(a != null) {
			shouldFail(var s:String = a);
		}
	}

	static function checkedAgainstNull_modifiedInClosureInLoop_shouldBecomeNeverSafe(?a:String) {
		for(i in 0...Std.random(10)) {
			trace(_ -> a = null);
		}
		if(a != null) {
			shouldFail(var s:String = a);
		}
	}

	static function checkedAgainstNull_modifiedInNestedClosure_shouldBecomeNeverSafe(?a:String) {
		trace(() -> () -> a = null);
		if(a != null) {
			shouldFail(var s:String = a);
		}
	}

	static function checkAgainstNull_complexConditions() {
		var nullable:Null<String> = 'hello';
		var s:String;
		if(nullable != null && true) {
			s = nullable;
		}
		else {
			shouldFail(s = nullable);
		}
		if(false && (true || false) && null == nullable) {
			shouldFail(s = nullable);
		} else {
			s = nullable;
		}
		if(true || nullable != null) {
			shouldFail(s = nullable);
		} else {
			shouldFail(s = nullable);
		}
	}

	static function checkAgainstNull_deadEndIfNull_shouldPassAfterCheckedBlock(?a:String, ?b:String) {
		if(a == null) {
			return;
		}
		//function execution will continue only if `a` is not null
		var s:String = a;

		if(b != null) {
		} else {
			throw "Dead end";
		}
		//function execution will continue only if `b` is not null
		var s:String = b;
	}

	static function checkAgainstNull_deadEndOfLoop_shouldPassAfterCheckedBlock(?a:String, ?b:String) {
		var s:String;
		while(Std.random(2) == 1) {
			shouldFail(s = a);
			if(a == null) continue;
			s = a;

			shouldFail(s = b);
			if(b == null) break;
			s = b;
		}
	}

	static function checkAgainstNull_valueBecomesSafeInIf_shouldStaySafe(?a:String) {
		if(a == null) {
			a = 'hello';
		}
		var s:String = a;
	}

	static function return_nullableValueFromNotNullableResult_shouldFail(?a:String):String {
		function local():String {
			shouldFail(return a);
		}
		shouldFail(return a);
	}

	static function objectDecl_fieldsExpressions_shouldBeChecked(?a:String) {
		var s:String;
		var o = {
			field: shouldFail(s = a)
		}
	}

	static function for_iterateOverNullableValue_shouldFail(?a:Iterable<Int>) {
		for(i in shouldFail(a)) {}
	}

	static function while_nullableCondition_shouldFail(?a:Bool) {
		shouldFail(while(a) {});
	}

	static function while_checkAgainstNullInConditionAndUseInBody(?a:Bool) {
		var b:Bool;
		while(a != null) b = a;
		do shouldFail(b = a) while(a != null);
		while(a == null) shouldFail(b = a);
	}

	static function throw_nullableValue_shouldFail() {
		var s:Null<String> = 'hello';
		shouldFail(throw s);
	}

	static function arrayDeclaration_shouldCheck(?a:String) {
		var s:String;
		shouldFail([s = a]);
	}

	static function arrayDeclaration_nullableItemInNotNullableArray_shouldFail(?s:String, ?i:Int) {
		var arr:Array<String>;
		shouldFail(arr = ['', s, '']);
		function local(a:Array<Int>)
		([1, shouldFail(i)]:Array<Int>);
	}

	static function tryCatch_shouldCheck(?a:String) {
		var s:String;
		try {
			shouldFail(s = a);
		} catch(e:Dynamic) {
			shouldFail(s = a);
		}
	}

	static function cast_nullableExprToNotNullableType_shouldFail() {
		var s:Null<String> = 'hello';
		shouldFail((s:String));
		shouldFail(cast(s, String));
	}

	static function untypedCast_shouldPass() {
		var n:Null<String> = 'hello';
		var s:String = cast n;
	}

	static function enum_switchOnNullableEnum_shouldFail(e:Null<DummyEnum>) {
		switch shouldFail(e) {
			case DummyOne:
			case DummyTwo(a, b):
		}
	}

	static function unification_typeOfNullableToTypeOfNotNullable_shouldFail(?a:Int) {
		var withNullables = [1, a, 2];
		shouldFail(var notNullables:Array<Int> = withNullables);
		var withNullables = [a => 1, 2 => 3];
		shouldFail(var notNullables:Map<Int,Int> = withNullables);
	}

	static function objectDecl_passObjWithNullabelFieldToObjWithNotNullableField_shouldFail(?a:String) {
		shouldFail(var o:{field:String} = {field:a});
		shouldFail(o = new Test('')); //Test has `field:Null<String>`
		var arr = (['', a]:Array<Null<String>>);
		shouldFail(var q:{field:Array<String>} = {field:arr});
		shouldFail(var v:{value:Array<String>} = new Generic(arr));
	}

	static function closure_whichReturnsWithoutExplicitType_shouldPass(s:String) {
		return
			function() {
				return s;
			};
	}

	static function switch_onNullableValue() {
		var nullable:Null<String> = 'hello';
		var s:String;

		switch(nullable) {
			case null:
				shouldFail(s = nullable);
			case _:
				s = nullable;
		};

		switch(nullable) {
			case v if(Std.random(2) == 1):
				shouldFail(s = v);
				shouldFail(s = nullable);
			case null:
				shouldFail(s = nullable);
			case v if(Std.random(2) == 1):
				s = v;
				s = nullable;
			case v:
				s = v;
				s = nullable;
		}
	}

	static function anonymousObjects() {
		var o:AnonAsClass = {};
		shouldFail(var s:String = o.optional);
		var o:AnonAsStruct = {};
		shouldFail(var s:String = o.optional);
	}

	static function safetyInference_safeValueAssignedToNullable_shouldBecomeSafe(?a:String, ?b:String) {
		a = 'hello';
		var s:String = a;
	}

	static function safetyInference_safeValueAssignedToNullableInAllBranches_shouldStaySafe(?a:String, ?b:String) {
		if(Std.random(2) == 1) {
			a = 'hello';
		} else {
			a = 'world';
		}
		var s:String = a;
	}

	static function closure_returnsSomethingAndMethodReturnsNullable_shouldPass():Null<String> {
		function local() return 10;
		return null;
	}

	static function functionWithNotNullableArg_toFunctionWithNullableArg_shouldFail() {
		var fn = function(s:String):Void {}
		var nullable = function(s:Null<String>):Void {}
		shouldFail(nullable = fn);
	}

	static function functionWithNullableArg_toFunctionWithNotNullableArg_shouldPass() {
		var fn = function(s:String):Void {}
		var nullable = function(s:Null<String>):Void {}
		fn = nullable;
	}

	static function functionWithNullableResult_toFunctionWithNotNullableResult_shouldFail() {
		var fn = function():String return '';
		var nullable = function():Null<String> return null;
		shouldFail(fn = nullable);
	}

	static function functionWithNotNullableResult_toFunctionWithNullableResult_shouldPass() {
		var fn = function():String return '';
		var nullable = function():Null<String> return null;
		nullable = fn;
	}
}