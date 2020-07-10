package db;

import sys.db.Sqlite;
import sys.db.Connection;
import sys.FileSystem;

class SqliteSetup extends utest.Test {
	static public final DB_FILE = 'temp/db.sqlite';

	var cnx:Connection;

	function setupClass() {
		try FileSystem.deleteFile(DB_FILE) catch(_) {}
	}

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
		cnx = Sqlite.open(DB_FILE);
	}
}
