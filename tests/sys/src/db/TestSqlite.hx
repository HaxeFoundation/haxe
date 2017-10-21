package db;

import sys.db.*;
import utest.Assert;
import sys.FileSystem;

class TestSqlite {
	static public inline var DB_FILE = 'temp/db.sqlite';

	var cnx:Connection;

	public function setup() {
		cnx = Sqlite.open(DB_FILE);
		cnx.request("CREATE TABLE test (id INTEGER PRIMARY KEY AUTOINCREMENT, num REAL, value TEXT)");
	}

	public function tearDown() {
		cnx.close();
		FileSystem.deleteFile(DB_FILE);
	}

	public function new() {}

	public function testCorrectDbFileCreated() {
		Assert.isTrue(FileSystem.exists(DB_FILE));
	}

	public function testResultSet() {
		cnx.request("INSERT INTO test (id, num, value) VALUES (1, 1.5, 'one'),(10, 10.1, 'ten')");

		var result = cnx.request("SELECT * FROM test ORDER BY id");
		Assert.isTrue(result.hasNext());
		var next = result.next();
		Assert.equals(1, next.id);
		Assert.equals(1.5, next.num);
		Assert.equals('one', next.value);
		Assert.equals('ten', result.getResult(2));
		Assert.equals(10, result.getIntResult(0));
		Assert.equals(10.1, result.getFloatResult(1));
		Assert.same(['id', 'num', 'value'], result.getFieldsNames());

		var result = cnx.request("SELECT * FROM test ORDER BY id");
		Assert.same([{id:1, num:1.5, value:'one'}, {id:10, num:10.1, value:'ten'}], Lambda.array(result.results()));
	}

	public function testLastInsertId() {
		cnx.request("INSERT INTO test (value) VALUES ('one')");
		Assert.equals(1, cnx.lastInsertId());

		cnx.request("INSERT INTO test (value) VALUES ('two')");
		Assert.equals(2, cnx.lastInsertId());
	}

	public function testDbName() {
		Assert.equals('SQLite', cnx.dbName());
	}

	public function testTransactionRollback() {
		cnx.startTransaction();
		cnx.request("INSERT INTO test (num, value) VALUES (1, 'one')");
		cnx.rollback();
		var result = cnx.request("SELECT * FROM test");
		Assert.isFalse(result.hasNext());
	}

	public function testTransactionCommit() {
		cnx.startTransaction();
		cnx.request("INSERT INTO test (num, value) VALUES (1, 'one')");
		cnx.commit();
		var result = cnx.request("SELECT * FROM test");
		Assert.isTrue(result.hasNext());
		Assert.same({id:1, num:1, value:'one'}, result.next());
	}
}