package unit.issues;

import unit.Test;

class Issue2244 extends Test {
	function test() {
		var x = "foo";
		var y = '${ '${x}'}';
		eq("foo", y);
	}

	function testQuote() {
		eq("'", '${'\''}');
	}

	function testDeeperQuote() {
		eq("'", '${'${'${'${'\''}'}'}'}');
	}

	function testAurel() {
		var foo = "hello";
		eq("hello", '${'${foo}'}');
		eq("\nhello", '\n${'${foo}'}');
	}

	function testJeremy() {
		eq("hello 'name' hello 'jeremy'", 'hello \'name\' ${(name -> 'hello \'$name\'')('jeremy')}');
	}

	function testIssue3974() {
		var s = "foo";
		var s2 = '\t${s.length}';
		eq("\t3", s2);
	}

	function testIssue5985() {
		eq("\n 0", '\n ${[].length}');
	}

	function testIssue8888() {
		eq("\\\\", '${"\\\\"}');
	}
}
