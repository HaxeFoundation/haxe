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

	function testIssue8728() {
		final DB_FILE = 'temp/db.sqlite';
		final DB_ROW_COUNT = 230;
		final VALUE = "abxs";

		if (sys.FileSystem.exists(DB_FILE))
			sys.FileSystem.deleteFile(DB_FILE);
		final cnx = sys.db.Sqlite.open(DB_FILE);
		cnx.request('CREATE TABLE tbl (id INTEGER PRIMARY KEY AUTOINCREMENT, value TEXT);');

		final insert = "INSERT INTO tbl (value) VALUES " + [for (_ in 0...DB_ROW_COUNT) '(\'$VALUE\')'].join(",");
		cnx.request(insert);
		cnx.close();

		// error was random, so repeat a few times for good measure
		for (_ in 0...20) {
			final rows = sys.db.Sqlite.open(DB_FILE).request("SELECT * FROM tbl");
			equals(DB_ROW_COUNT, rows.length);
		}
	}

	function testNfields() {
		var result = cnx.request('SELECT * FROM test');
		equals(3, result.nfields);
	}

	function testResults() {
		var result = cnx.request('SELECT * FROM test');
		same(data, Lambda.array(result.results()));
	}

#if !cpp //ResultSet.getFieldsNames() is not implemented in cpp.

	function testGetFieldsNames() {
		var result = cnx.request('SELECT * FROM test ORDER BY id');
		same(['id', 'num', 'value'], result.getFieldsNames());
	}
#end

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