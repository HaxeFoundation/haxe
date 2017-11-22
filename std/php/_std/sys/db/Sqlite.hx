/*
 * Copyright (C)2005-2017 Haxe Foundation
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

import php.*;
import php.db.*;
import sys.db.*;

@:coreApi class Sqlite {
	public static function open( file:String ) : Connection {
		return new SQLiteConnection(file);
	}
}

private class SQLiteConnection implements Connection {
	var db:SQLite3;

	public function new( file:String ) {
		db = new SQLite3(file);
		db.enableExceptions(true);
	}

	public function request( s : String ) : ResultSet {
		var result = db.query(s);
		return new SQLiteResultSet(result);
	}

	public function close() : Void {
		db.close();
	}

	public function escape( s : String ) : String {
		return SQLite3.escapeString(s);
	}

	public function quote( s : String ) : String {
		if (s.indexOf("\000") >= 0) return "x'" + Global.bin2hex(s) + "'";
		return "'" + SQLite3.escapeString(s) + "'";
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
		return Syntax.int(db.lastInsertRowID());
	}

	public function dbName() : String {
		return 'SQLite';
	}

	public function startTransaction() : Void {
		db.query('BEGIN TRANSACTION');
	}

	public function commit() : Void {
		db.query('COMMIT');
	}

	public function rollback() : Void {
		db.query('ROLLBACK');
	}
}

private class SQLiteResultSet implements ResultSet {
	public var length(get,null) : Int;
	public var nfields(get,null) : Int;

	var _length : Int = 0;
	var _nfields : Int = 0;

	var loaded:Bool = false;
	var currentIndex:Int = 0;
	var rows:NativeIndexedArray<NativeAssocArray<Scalar>>;
	var result:SQLite3Result;
	var fetchedRow:NativeArray;
	var fieldsInfo:NativeAssocArray<Int>;

	public function new( result:SQLite3Result ) {
		this.result = result;
	}

	public function hasNext() : Bool {
		if (!loaded) load();
		return currentIndex < _length;
	}

	public function next() : Dynamic {
		if (!loaded) load();
		var next:Dynamic = rows[currentIndex++];
		return Boot.createAnon(correctArrayTypes(next));
	}

	public function results() : List<Dynamic> {
		if (!loaded) load();
		var list = new List();
		Syntax.foreach(rows, function(_, row) list.add(Boot.createAnon(correctArrayTypes(row))));
		return list;
	}

	public function getResult( n : Int ) : String {
		if (!loaded) load();
		if (!hasNext()) return null;
		return Global.array_values(rows[currentIndex])[n];
	}

	public function getIntResult( n : Int ) : Int {
		return Syntax.int(getResult(n));
	}

	public function getFloatResult( n : Int ) : Float {
		return Syntax.float(getResult(n));
	}

	public function getFieldsNames() : Null<Array<String>> {
		var fieldsInfo = getFieldsInfo();
		return Global.array_keys(fieldsInfo);
	}

	function correctArrayTypes(row:NativeAssocArray<String>):NativeAssocArray<Scalar> {
		var fieldsInfo = getFieldsInfo();
		Syntax.foreach(row, function(field:String, value:String) {
			row[field] = correctType(value, fieldsInfo[field]);
		});
		return cast row;
	}

	inline function getFieldsInfo():NativeAssocArray<Int> {
		if (fieldsInfo == null) {
			fieldsInfo = cast Syntax.arrayDecl();
			for(i in 0...nfields) {
				fieldsInfo[result.columnName(i)] = result.columnType(i);
			}
		}
		return fieldsInfo;
	}

	function load() {
		loaded = true;
		_nfields = result.numColumns();
		getFieldsInfo();
		fetchAll();
	}

	function correctType(value:String, type:Int):Scalar {
		if (value == null) return null;
		if (type == Const.SQLITE3_INTEGER) return Syntax.int(value);
		if (type == Const.SQLITE3_FLOAT) return Syntax.float(value);
		return value;
	}

	function fetchAll() {
		rows = Syntax.arrayDecl();
		var index = 0;
		var row = result.fetchArray(Const.SQLITE3_ASSOC);
		while(row != false) {
			rows[index] = correctArrayTypes(row);
			row = result.fetchArray(Const.SQLITE3_ASSOC);
			index++;
		}
		_length = index;
	}

	function get_length() return _length;
	function get_nfields() return _nfields;
}