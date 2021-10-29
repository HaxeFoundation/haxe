package cases;

import Validator.shouldFail;

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
	var ?optional:String;
}

typedef AnonAsStruct = {
	?optional:String
}

/** Test `@:nullSafety(Off)` is respected on fields */
class UnsafeFields {
	@:nullSafety(Off) var unsafeVar:String = null;
	@:nullSafety(Off) var notInitializedField:String;

	@:nullSafety(Off)
	static function unsafeMethod() {
		var s:String = null;
	}

	static function unsafeExpr() {
		var s:String;
		@:nullSafety(Off) cast(null, String);
	}

	var str(get,set):String;
	@:nullSafety(Off) function get_str() return (null:Null<String>);
	@:nullSafety(Off) function set_str(v) return (v:Null<String>);
}

/** Test `@:nullSafety(Off)` is respected on a class */
@:nullSafety(Off)
class UnsafeClass {
	var uninitializedVar:String;

	public function new() {
		doStuff(this); //pass this somewhere before all fields are initialized
	}

	function doStuff(t:UnsafeClass) {}
}

/** Test `@:nullSafety(Off)` on a constructor. And that it does not disable safety checks for instance vars */
@:build(Validator.checkFields())
class UnsafeConstructor {
	@:shouldFail var uninitializedVar:String;

	@:nullSafety(Off)
	public function new() {
		var s:String = null;
	}
}

@:build(Validator.checkFields())
private class TestWithoutConstructor {
	@:shouldFail var notInitializedField:String;
}

class Issue9643 {
	static var tmp:Null<()->Void>;

	final field: String;

	public function new() {
		tmp = () -> @:nullSafety(Off) method();
		field = 'hello';
	}

	function method() {}
}

class AllVarsInitializedInConstructor_weHaveClosure_thisShouldBeUsable {
	var v:Int;

	/**
	 * This is generated like:
	 * ```haxe
	 * var _gthis = this; //problems come from here
	 * this.v = 42;
	 * var f = function() {
	 * 		return _gthis.v;
	 * }
	 * ```
	 */
	public function new() {
		v = 42;
		var f = function() return this.v;
	}
}

@:build(Validator.checkFields())
class TestStrict {
	extern static final something:String;

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

	var str(get,set):String;
	function get_str() {
		shouldFail(return (null:Null<String>));
	}
	function set_str(v) {
		shouldFail(return (v:Null<String>));
	}

	/**
	 *  Null safety should work in __init__ functions
	 */
	static function __init__() {
		var s:Null<String> = null;
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
		shouldFail(acceptThis(this));
		@:nullSafety(Off) acceptThis(this);
		var self = this;
		shouldFail(acceptThis(self));
		shouldFail(instanceMethod());
		var closure = shouldFail(instanceMethod);
		var notInitializedYet = shouldFail(initializedInConstructor);
		initializedInConstructor = 'hello';
		var s:Null<String> = null;
		shouldFail(s.length);
	}

	static function acceptThis(t:TestStrict) {}

	function instanceMethod() {}

	static function fieldAccess_onNullableValue_shouldFail():Void {
		var a:Null<String> = null;
		shouldFail(a.length);
	}

	static function fieldAccess_onNullableValueInIfCondition_shouldFail():Void {
		var a:Null<String> = null;
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
		var fn:Null<()->Void> = null;
		shouldFail(fn());
	}

	static function call_onNotNullableValue_shouldPass() {
		var fn:()->Void = function() {}
		fn();
	}

	static function call_nullableValueToNotNullableArgument_shouldFail() {
		var fn = function(a:String) {}
		var v:Null<String> = null;
		shouldFail(fn(v));
	}

	static function call_nullableValueToOptionalArgument_shouldPass() {
		var fn = function(?a:Int) {}
		var v:Null<Int> = null;
		fn(v);
	}

	static public function new_nullableValueToNotNullableArgument_shouldFail(?v:String) {
		shouldFail(new TestStrict(v));
	}

	static public function new_nullableValueToNotNullableGenericArg_shouldFail(?n:String) {
		shouldFail(new Generic<String>(n));
	}

	static public function new_nullableValueToNullableGenericArg_shouldPass(?n:String) {
		new Generic(n);
	}

	static function varDecl_assignNullableValueToNotNullableVar_shouldFail() {
		var v:Null<String> = null;
		shouldFail(var s:String = v);
		shouldFail(var s:String = null);
	}

	static function unsafeNullableVar_assignedToNonNullablePlases_shouldPass() {
		var @:nullSafety(Off) n:Null<String> = null;
		var s:String = n;
		function test(s:String) {}
		test(n);
	}

	static function assign_nullableValueToNotNullable_shouldFail() {
		var a:Null<Int> = null;
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
		var a:Null<Int> = null;
		var b = 10;
		shouldFail(a + b);
	}

	static function binop_comparisonWithNullableValue_shouldPass() {
		var a:Null<Int> = null;
		var b = 10;
		a == b;
		a != b;
	}

	static function unop_nullableValue_shouldFail() {
		var a:Null<Int> = null;
		shouldFail(a++);
	}

	static function ternary_nullableElse_assignToNotNullableValue_shouldFail() {
		var v:Null<String> = null;
		var a:String;
		shouldFail((true ? 'hello' : v).length);
	}

	static function ternary_returnedFromInlinedFunction_shouldPass() {
		var str:String = inlinedNullableSafeString([null]);
	}

	static inline function inlinedNullableSafeString(nullables:Array<Null<String>>):String {
		var s = nullables[0];
		return (s != null ? s : 'hello' );
	}

	static function arrayAccess_nullableArray_shouldFail() {
		var a:Null<Array<Int>> = null;
		shouldFail(a[0]);
	}

	static function arrayAccess_usingNullableIndex_shouldFail() {
		var a:Array<Int> = [0];
		var idx:Null<Int> = null;
		shouldFail(a[idx]);
	}

	static function if_nullableCondition_shouldFail() {
		var s:Null<Bool> = null;
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
		if(!(a == null)) s = a;
		if(!(a == null || a == a)) a.charAt(0);
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

	static function checkAgainstNull_checkAndFieldAccess(?a:String, ?s:String) {
		if(s != null && s.length == 0) {}
		if(s == null || s.length == 0) {}
		s != null
			&& s.length == 0
			&& s.length == 0;
		s == null
			|| s.length == 0
			|| s.length == 0;
		!(s == null || a == null) && s.length == a.length;

		shouldFail(if(s != null || s.length == 0) {});
		shouldFail(if(s == null && s.length == 0) {});
		shouldFail(s != null || s.length == 0);
		shouldFail(s == null && s.length == 0);
	}

	static function checkedAgainstNotNullableValue(?a:String) {
		var s = 'world';
		if(a == s) {
			a.charAt(0);
		} else {
			shouldFail(a.charAt(0));
		}
		// if(a != s) {
		// 	shouldFail(a.charAt(0));
		// } else {
		// 	a.charAt(0);
		// }
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
		var nullable:Null<String> = null;
		var s:String;
		if(nullable != null && true) {
			s = nullable;
		} else {
			shouldFail(s = nullable);
		}
		if(Std.random(2) == 1 && null == nullable) {
			shouldFail(s = nullable);
		} else {
			shouldFail(s = nullable);
		}
		if(Std.random(2) == 1 || null == nullable) {
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

	static public function checkAgainstNul_deadEndIfAnyIsNull_shouldPass(?a:String, ?b:String) {
		if (a == null || b == null) {
			return;
		}

		a.length + b.length;
	}

	public function checkAgainstNul_deadEndIfNullOrAnotherCondition_shouldPass() : Void {
		var s : Null<String> = ((Math.random() > 0.5) ? "A" : null);

		if (s == null || s.length == 42) {
			return;
		}

		s.length;
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

	static function while_checkAgainstNullInConditionAndReassignInBody(?a:{value:String, ?parent:Dynamic}) {
		while (a != null) {
			var s:String = a.value;
			a = a.parent;
			shouldFail(a.value);
		}

		do {
			a = shouldFail(a.parent);
		} while (a != null);

		a = {value:'hello', parent:null};
		do {
			var s:String = a.value;
			a = a.parent;
			shouldFail(a.value);
		} while (a != null);

		shouldFail(a.value);
	}

	static function throw_nullableValue_shouldFail() {
		var s:Null<String> = null;
		shouldFail(throw s);
	}

	static function arrayDeclaration_shouldCheck(?a:String) {
		var s:String;
		shouldFail(([s = a]:Array<Null<String>>));
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
		var s:Null<String> = null;
		shouldFail((s:String));
		shouldFail(cast(s, String));
	}

	static function untypedCast_shouldPass() {
		var n:Null<String> = null;
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
		shouldFail(o = new TestStrict('')); //Test has `field:Null<String>`
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
		var nullable:Null<String> = null;
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
		var n:Null<String> = 'world';
		s = n;
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

		var fn = function(a:Array<String>):Void {}
		var nullable = function(a:Array<Null<String>>):Void {}
		shouldFail(nullable = fn);

		var fn = function(o:{field:String}):Void {}
		var nullable = function(o:{field:Null<String>}):Void {}
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

	static function functionWithNullableReturnType_toVoidFunction_shouldPass() {
		var n:()->Null<String> = () -> null;
		var f:()->Void = n;
	}

	static public function tryBlock_couldNotBeDeadEndForOuterBlock() {
		var c:Null<String> = null;
		try {
			if (c == null) {
				throw "null";
			}
			c.charAt(0);
		} catch (e:Dynamic) {
			shouldFail(c.charAt(0));
		}
	}

	var foo:Null<String>;
	public function localNamedFunction_shouldPass() {
		function cb() {
			if(foo != null) {
				cb();
			}
		}
	}

	static public function closure_immediatelyExecuted_shouldInheritSafety(?s:String) {
		if(s != null) {
			[1, 2, 3].map(i -> i * s.length);
			pureMeta(() -> s.length);
			notPureButImmediatelyExecutes(() -> s.length);
			immediatelyExecutesTwoLevelsDeep(() -> s.length);
			pureMeta(function() {
				shouldFail(s.charAt(0));
				s = null;
				return shouldFail(s.length);
			});
			pureMeta(() -> shouldFail(s.length));
			shouldFail(s.charAt(0));
		}
	}
	@:pure
	static function pureMeta(cb:()->Int) return cb();
	static var tmp1:Int = Std.random(10);
	static function notPureButImmediatelyExecutes(cb:()->Int) {
		if(tmp1 < 5) tmp1 = cb();
		for(i in 0...cb()) tmp1 += cb();
	}
	static function immediatelyExecutesTwoLevelsDeep(cb:()->Int) {
		notPureButImmediatelyExecutes(cb);
	}

	static public function closure_storedSomewhere_shouldFail(?s:String) {
		if(s != null) {
			passesSomewhereElse(() -> shouldFail(s.length));
			storesSomewhere(() -> shouldFail(s.length));
		}
	}
	static function passesSomewhereElse(cb:()->Int) {
		haxe.Timer.delay(cb, 1);
	}
	static var tmp2:Null<()->Int> = null;
	static function storesSomewhere(cb:()->Int) {
		tmp2 = cb;
	}

	static function closure_passedToRecursiveFunction_shouldNotCrashTheCompiler(?a:String) {
		if(a != null) {
			recursive(() -> a.length);
		}
	}
	static function recursive(cb:()->Int) {
		if(Std.random(10) == 0) {
			recursive(cb);
		} else {
			cb();
		}
	}

	static function closure_passedToOverriddenMethod(?a:String) {
		var o:Parent = new Child();
		if(a != null) {
			cast(o, Child).childExecute(() -> a.length);
			o.execute(() -> shouldFail(a.length));
		}
	}

	static function recursiveTypedef_shouldNotCrashTheCompiler(a:Recursive<Void>, b:Recursive<Void>) {
		a = b;
	}

	function nonFinalField_immediatelyAfterCheck_shouldPass(o:{field:Null<String>}) {
		if(o.field != null) {
			var notNullable:String = o.field;
		}
	}

	function nonFinalField_afterLocalAssignment_shouldPass(o:{field:Null<String>}, b:{field:Null<String>}) {
		if(o.field != null) {
			b = {field:null};
			var notNullable:String = o.field;
		}
	}

	function nonFinalField_afterFieldAssignment_shouldFail(o:{field:Null<String>}, b:{o:{field:Null<String>}}) {
		if(o.field != null) {
			b.o = {field:null};
			shouldFail(var notNullable:String = o.field);
		}
	}

	function nonFinalField_afterSomeCall_shouldFail(o:{field:Null<String>}) {
		if(o.field != null) {
			someCall();
			shouldFail(var notNullable:String = o.field);
		}
	}
	function someCall() {}

	static function anonFinalNullableField_checkedForNull() {
		var o:{ final ?f:String; } = {};
		if (o.f != null) {
			var s:String = o.f;
			o = {};
			shouldFail(var s:String = o.f);
		}
	}

	static function staticFinalNullableField_checkedForNull() {
		if (FinalNullableFields.staticVar != null) {
			var s:String = FinalNullableFields.staticVar;
		}
		shouldFail(var s:String = FinalNullableFields.staticVar);
	}

	static function return_assignNonNullable_shouldPass(?n:String):String {
		return n = 'hello';
	}

	static function stringConcat_shouldPass(?a:String) {
		'hello, ' + a;
		a += 'hello';
	}

	static function stringConcat_twoNullables_shouldFail(?a:String, ?b:String) {
		shouldFail(a + b);
		shouldFail(a += b);
	}

	static function anonFields_checkedForNull() {
		var i:Null<Int> = null;
		shouldFail(({a: i} : {a:Int}));
		if (i != null) {
			({a: i} : {a:Int});
			({a: i} : {a:Null<Int>});
			({a: 0, b: i} : {a:Int, b: Int});
		}
	}

	static function immediateFunction_keepsSafety(?s:String) {
		if (s != null) {
			(function() s.length)();
		}
	}

	static function fieldAccess_onBlockWithSafeVarDeclaredInside_shouldPass(?a:String) {
		var fn = function() {
			({
				var value = a;
				if(value == null)
					'hello'
				else
					value;
			}).length;
		}
	}

	function safetyOffArgument_shouldPass(?a:String) {
		staticSafetyOffArgument(a);
		instanceSafetyOffArgument(a);
		inline instanceSafetyOffArgument(a);
	}
	static function staticSafetyOffArgument(@:nullSafety(Off) b:Dynamic) {}
	function instanceSafetyOffArgument(@:nullSafety(Off) b:Dynamic) {
		return staticSafetyOffArgument(b);
	}

	static function issue8122_abstractOnTopOfNullable() {
		var x:NullFloat = null;
		var y:Float = x.val();
		x += x;
	}

	static function issue9649_nullCheckedAbstractShouldUnify_shouldPass() {
		var x:NullFloat = null;
		var y:Float = 0.0;
		if(x!=null) y = x;
	}

	static function issue8443_nullPassedToInline_shouldPass() {
		inline function method(?map: (Int)->Int) {
			return map != null ? map(0) : -1;
		}

		var x:Int = method();
	}

	static function issue7900_trace() {
		var x:Null<()->String> = null;
		trace(x);
		trace("hi", x);
		trace("hi", shouldFail(x()));
	}

	@:shouldFail @:nullSafety(InvalidArgument)
	static function invalidMetaArgument_shouldFail() {}

	static function issue9474_becomesSafeInIf() {
		var a:Null<String> = null;
		if(Math.random() > 0.5) a = 'hi';
		shouldFail(var s:String = a);

		var a:Null<String> = null;
		if(Math.random() > 0.5) a = null
		else a = 'hello';
		shouldFail(var s:String = a);

		var a:Null<String> = null;
		if(Math.random() > 0.5) a = 'hello'
		else a = null;
		shouldFail(var s:String = a);

		var a:Null<String> = null;
		if(a == null) a = 'hi';
		var s:String = a;

		var a:Null<String> = null;
		if(Math.random() > 0.5) a = 'hi'
		else a = 'hello';
		var s:String = a;
	}

	/**
	 * @see https://github.com/HaxeFoundation/haxe/pull/10428#issuecomment-951574457
	 */
	static function issue10428() {
		final arr:Array<Int> = [
			{
				var tmp = (1 : Null<Int>);
				if (tmp != null) tmp else 2;
			}
		];
	}
}

private class FinalNullableFields {
	static public final staticVar:Null<String> = "hello";
	public final instanceVar:Null<String> = "world";

	function instanceFinalNullableField_checkedForNull() {
		if (instanceVar != null) {
			var s:String = instanceVar;
		}
		shouldFail(var s:String = instanceVar);
	}
}

typedef Recursive<T1> = {
	public function rec<T2>(a:Recursive<T1>):Recursive<T2>;
}

// @see https://github.com/HaxeFoundation/haxe/issues/7733
// class RecClass<T1> {
// 	public function rec<T2>(a:Recursive<T1>):Recursive<T2> return a;
// }

private class Parent {
	public function new() {}

	public function execute(cb:()->Void) cb();
}

private class Child extends Parent {
	static var tmp:Any = '';
	override public function execute(cb:()->Void) tmp = cb;
	public function childExecute(cb:()->Void) cb();
}

abstract NullFloat(Null<Float>) from Null<Float> to Null<Float> {
	public inline function val(): Float {
		return this != null ? this : 0.0;
	}

	@:op(A + B) static inline function addOp1(lhs: NullFloat, rhs: Float): Float {
		return lhs != null ? lhs.val() + rhs : rhs;
	}
}