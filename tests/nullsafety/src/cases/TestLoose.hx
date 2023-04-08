package cases;

import Validator.shouldFail;

typedef NotNullAnon = {
	a:String
}

typedef NotNullAnonRef = NotNullAnon;

class TestLoose {
	static var staticVar:Null<String>;
	var instanceVar:Null<String>;

	function mutate(o:{field:Null<String>}) {
		o.field = null;
	}

	function possibleMutation_shouldPass(o:{field:Null<String>}) {
		if(o.field != null) {
			mutate(o);
			var notNullable:String = o.field;
		}
	}

	static function staticNullableField_checkedForNull() {
		if (staticVar != null) {
			var s:String = staticVar;
		}
		shouldFail(var s:String = staticVar);
	}

	function instanceNullableField_checkedForNull() {
		if (instanceVar != null) {
			var s:String = instanceVar;
		}
		shouldFail(var s:String = instanceVar);
	}

	function explicitMutation_shouldFail(o:{field:Null<String>}) {
		if(o.field != null) {
			o = {field:null};
			shouldFail(var notNullable:String = o.field);
		}
	}

	function nullable_assignNotNull_shouldPass(o:{field:Null<String>}) {
		o.field = 'hello';
		var notNullable:String = o.field;
	}

	function safeExpr_assignNull_shouldFail(o:{field:Null<String>}) {
		if(o.field != null) {
			o.field = null;
			shouldFail(var notNullable:String = o.field);
		}
	}

	function nestedField_checkedNotNull_shouldPass(o:{o2:{field:Null<String>}}) {
		if(o.o2.field != null) {
			var notNullable:String = o.o2.field;
		}
	}

	function checkedAgainstNullInLocalFunction_shouldPass(?callback:()->Void) {
		function bar() {
			if (callback != null) {
				callback();
			}
		}
		bar();
	}

	static var struct:Null<{foo:String}>;

	static function checkAgainstNull_checkAndFieldAccess() {
		struct == null
			|| struct.foo != ""
			|| struct.foo != "";
		struct != null
			&& struct.foo != ""
			&& struct.foo != "";
	}

	static function safeNullable_capturedInLocalFunction_shouldPass(?a:String) {
		if(a == null) {
			return;
		}
		var fn = function():String {
			return a;
		}
	}

	static function objectDecl_nullInField_shouldFail():Void {
		shouldFail({
			final value:Null<{a:String}> = {a: null};
		});
		final value:Map<Int, Null<{a:String}>> = [
			1 => shouldFail({a: null})
		];
	}

	static function objectDecl_nullInTypedef_shouldFail():Void {
		shouldFail({
			final value:NotNullAnon = {a: null};
		});
		final value:Map<Int, Null<NotNullAnon>> = [
			1 => shouldFail({a: null})
		];
		var value:NotNullAnon = {a: ""};
		shouldFail(value = {a: null});

		final value:Map<Int, Null<NotNullAnonRef>> = [
			1 => shouldFail({a: null})
		];
		var value:Null<NotNullAnonRef> = {a: ""};
		shouldFail(value = {a: null});
	}

	static function testIssue8442() {
		function from(array: Array<Float>) {
			return array.length;
		}

		function create(?array: Array<Float>) {
			return from(shouldFail(array));
			// haxe seems to think this unused null check means array is non-nullable
			if (array != null) {
			} else {
				return -1;
			}
		}
	}

	static function nullCoal_returnNull_shouldPass(token:{children:Array<Int>}):Null<Bool> {
		final children = token.children ?? return null;
		var i = children.length;
		return null;
	}

	static function localFunc_returnNullCoal_shouldFail():Void {
		function foo() {
			final x = (null : Null<Bool>) ?? return null;
			return x;
		}
		shouldFail(if (foo()) {});
	}
}
