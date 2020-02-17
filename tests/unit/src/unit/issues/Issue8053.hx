package unit.issues;

#if php
import php.Syntax.*;
import php.Const.*;
#end

class Issue8053 extends unit.Test {
	function test() {
		var b = "";

		#if !php
		var a:Dynamic = { };
		Reflect.setField(a, b, 1);
		#else
		var a:Dynamic = if(PHP_VERSION_ID < 70100) {
			//Prior to PHP 7.1.0 there was no way to add a property with an empty name to an existing object.
			//So, we create an object with an empty property name by transforming an associative array with an empty string index.
			object(assocDecl({"" : 1}));
		} else {
			var a = {};
			Reflect.setField(a, b, 1);
			a;
		}
		#end

		eq(1, Reflect.field(a, b));
	}
}