package db;

import utest.Assert.*;
import sys.FileSystem;

class TestSqliteConnection extends SqliteSetup {
	function testDbName() {
		equals('SQLite', cnx.dbName());
	}

	function testCorrectDbFileCreated() {
		isTrue(FileSystem.exists(SqliteSetup.DB_FILE));
	}

	function testLastInsertId() {
		cnx.request('INSERT INTO test (num, value) VALUES (10, "one")');
		equals(1, cnx.lastInsertId());

		cnx.request('INSERT INTO test (num, value) VALUES (20, "two")');
		equals(2, cnx.lastInsertId());
	}

	function testTransactionRollback() {
		cnx.startTransaction();
		cnx.request('INSERT INTO test (id, num, value) VALUES (1, 2, "one")');
		cnx.rollback();
		var result = cnx.request('SELECT * FROM test WHERE id = 1');
		isFalse(result.hasNext());
	}

	function testTransactionCommit() {
		cnx.startTransaction();
		cnx.request('INSERT INTO test (id, num, value) VALUES (1, 2, "one")');
		cnx.commit();
		var result = cnx.request('SELECT * FROM test WHERE id = 1');
		same({id:1, num:2, value:'one'}, result.next());
	}

	function testIssue9700() {
		function checkDataSaved(id:Int) {
			cnx.request('INSERT INTO test (id, num, value) VALUES ($id, $id, "dummy")');
			closeConnection();
			openConnection();
			var result = cnx.request('SELECT * FROM test WHERE id = $id');
			same({id:id, num:id, value:'dummy'}, result.next());
		}

		cnx.startTransaction();
		cnx.commit();
		checkDataSaved(1);

		cnx.startTransaction();
		cnx.rollback();
		checkDataSaved(2);
	}
}