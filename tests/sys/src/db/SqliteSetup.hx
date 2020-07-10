package db;

import sys.db.Sqlite;
import sys.db.Connection;
import sys.FileSystem;

class SqliteSetup extends utest.Test {
	var cnx:Connection;

	function setup() {
		openConnection();
		cnx.request("CREATE TABLE test (id INTEGER PRIMARY KEY AUTOINCREMENT, num REAL, value TEXT)");
	}

	function teardown() {
		cnx.request("DROP TABLE test");
		closeConnection();
	}

	function closeConnection() {
		cnx.close();
	}

	function openConnection() {
		cnx = Sqlite.open(':memory:');
	}
}
