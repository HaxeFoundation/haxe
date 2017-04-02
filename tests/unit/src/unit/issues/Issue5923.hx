package unit.issues;

#if php7
import php.Syntax.*;
#end

class Issue5923 extends unit.Test {
#if php7
	function test() {
		//These expressions should not fail at runtime
		binop({}, '??', null).value = 1;
		object(arrayDecl()).value = 1;
		array({})['value'] = 1;

		t(true);
	}
#end
}