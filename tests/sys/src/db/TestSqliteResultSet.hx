package db;

import utest.Assert.*;
import haxe.ds.ReadOnlyArray;

class TestSqliteResultSet extends SqliteSetup {
	var data:ReadOnlyArray<{id:Int, num:Float, value:String}> = [
		{id:1, num:1.5, value:'one'},
		{id:10, num:10.1, value:'ten'},
	];

	override function setup() {
		super.setup();
		var values = data.map(r -> '(${r.id}, ${r.num}, "${r.value}")').join(',');
		cnx.request('INSERT INTO test (id, num, value) VALUES $values');
	}

	function testLength() {
		var result = cnx.request('SELECT * FROM test');
		var length = result.length;

		result.next();
		equals(length - 1, result.length);

		for(_ in result) {}
		equals(0, result.length);
	}

	function testNfields() {
		var result = cnx.request('SELECT * FROM test');
		equals(3, result.nfields);
	}

	function testResults() {
		var result = cnx.request('SELECT * FROM test');
		same(data, Lambda.array(result.results()));
	}

	function testGetFieldsNames() {
		var result = cnx.request('SELECT * FROM test ORDER BY id');
		same(['id', 'num', 'value'], result.getFieldsNames());
	}

	function testAsIterator() {
		var result = cnx.request('SELECT * FROM test ORDER BY id');
		var rows = [];
		var expected = data.copy();
		for(row in result) {
			same(expected.shift(), row);
		}
	}

	function testGetResult() {
		var result = cnx.request('SELECT * FROM test ORDER BY id');
		equals('1', result.getResult(0));
		equals(1, result.getIntResult(0));
		equals(1, result.getFloatResult(0));
	}
}