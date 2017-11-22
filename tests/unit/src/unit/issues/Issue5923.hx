package unit.issues;

#if php
import php.Syntax.*;
#end

class Issue5923 extends unit.Test {
#if php
	function test() {
		//These expressions should not fail at runtime
		coalesce(({}:Dynamic), null).value = 1;
		object(arrayDecl()).value = 1;
		array({})['value'] = 1;

		t(true);
	}
#end
}