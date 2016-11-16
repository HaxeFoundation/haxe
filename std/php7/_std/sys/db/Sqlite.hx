/*
 * Copyright (C)2005-2016 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */
package sys.db;

import php7.*;
import php7.db.*;
import sys.db.*;

@:coreApi class Sqlite {
	public static function open( file:String ) : Connection {
		return new SQLiteConnection(file);
	}
}

private class SQLiteConnection implements Connection {
	var db:SQLiteDatabase;

	public function new( file:String ) {
		db = new SQLiteDatabase(file);
		if (db.lastError() != 0) {
			throw Global.sqlite_error_string(db.lastError());
		}
	}

	public function request( s : String ) : ResultSet {
		var error:String = Boot.deref(null);
		var result = db.query(s, Const.SQLITE_ASSOC, error);
		if (result == false) throw error;

		return new SQLiteResultSet(result);
	}

	public function close() : Void {
		Global.unset(db);
	}

	public function escape( s : String ) : String {
		return Global.sqlite_escape_string(s);
	}

	public function quote( s : String ) : String {
		return "'" + Global.sqlite_escape_string(s) + "'";
	}

	public function addValue( s : StringBuf, v : Dynamic ) : Void {
		if (Global.is_int(v) || Global.is_null(v)) {
			s.add(v);
		} else if (Global.is_bool(v)) {
			s.add(v ? 1 : 0);
		} else {
			s.add(quote(Std.string(v)));
		}
	}

	public function lastInsertId() : Int {
		return db.lastInsertRowid();
	}

	public function dbName() : String {
		return 'SQLite';
	}

	public function startTransaction() : Void {
		var success = db.queryExec('BEGIN TRANSACTION');
		if (!success) throw 'Failed to start transaction';
	}

	public function commit() : Void {
		var success = db.queryExec('COMMIT');
		if (!success) throw 'Failed to commit transaction';
	}

	public function rollback() : Void {
		var success = db.queryExec('ROLLBACK');
		if (!success) throw 'Failed to rollback transaction';
	}

}

private class SQLiteResultSet implements ResultSet {
	static var hxAnonClassName = Boot.getHxAnon().phpClassName;

	public var length(get,null) : Int;
	public var nfields(get,null) : Int;

	var result:SQLiteResult;
	var fetchedRow:NativeArray;

	public function new( result:SQLiteResult ) {
		this.result = result;
	}

	public function hasNext() : Bool {
		if (fetchedRow == null) fetchNext();
		return fetchedRow == null;
	}

	public function next() : Dynamic {
		if (fetchedRow == null) fetchNext();
		return withdrawFetched();
	}

	public function results() : List<Dynamic> {
		var list = new List();

		result.rewind();
		var row = result.fetchObject(hxAnonClassName);
		while (row) list.add(row);

		return list;
	}

	public function getResult( n : Int ) : String {
		if (fetchedRow == null) fetchNext();
		return Global.array_values(fetchedRow)[n];
	}

	public function getIntResult( n : Int ) : Int {
		return Syntax.int(getResult(n));
	}

	public function getFloatResult( n : Int ) : Float {
		return Syntax.float(getResult(n));
	}

	public function getFieldsNames() : Null<Array<String>> {
		return [for (i in 0...result.numFields()) result.fieldName(i)];
	}

	function fetchNext() {
		var next = result.fetch(Const.SQLITE_ASSOC);
		fetchedRow = (next == false ? fetchedRow = null : next);
	}

	function withdrawFetched() : Dynamic {
		if (fetchedRow == null) return null;
		return Boot.createAnon(fetchedRow);
	}

	function get_length() return result.numRows();
	function get_nfields() return result.numFields();
}