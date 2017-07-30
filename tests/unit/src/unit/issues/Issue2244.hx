package unit.issues;

import unit.Test;

class Issue2244 extends Test{
	function test() {
		var name = "John", age = 42;
		eq('Hello ${name+'$age years'} old', "Hello John42 years old");

		var x = "foo";
		var y = '${ '${x}'}';
		eq(y, "foo");

		eq('${'\''}', "'");
	}
}