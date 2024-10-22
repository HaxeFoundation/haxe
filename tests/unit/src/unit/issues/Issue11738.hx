package unit.issues;

import unit.Test;

class Issue11738 extends Test {
	var value:String;

	function test() {
		switch ("abc") {
			case _.charCodeAt(0) => 'a'.code:
				doSomething("1");
			case _.charCodeAt(1) => 'e'.code:
				doSomething("2");
		}
		eq("1", value);
	}

	function doSomething(s:String) {
		value = s;
	}
}
