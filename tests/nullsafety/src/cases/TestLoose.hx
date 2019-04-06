package cases;

import Validator.shouldFail;

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
}