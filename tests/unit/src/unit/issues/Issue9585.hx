package unit.issues;

using StringTools;

function getFloat():Dynamic {
	return 12.34;
}

function getInt():Dynamic {
	return 666;
}

function getBool():Dynamic {
	return false;
}

function getArray():Dynamic {
	return [1, 2, 3];
}

function getObject():Dynamic {
	return {x: 0};
}

class Issue9585 extends Test {
	function test() {
		var value1 /* :Dynamic */ = getFloat();
		var value2 /* :Dynamic */ = getInt();
		var value3 /* :Dynamic */ = getBool();
		var value4 /* :Dynamic */ = getArray();
		var value5 /* :Dynamic */ = getObject();

		var s:String = "Results: " + value1 + ", " + value2 + ", " + value3 + ", " + value4 + ", " + value5;
		t(s.startsWith("Results: ")); // We don't care about *exact* printing, but `s` should still be generated
	}
}
