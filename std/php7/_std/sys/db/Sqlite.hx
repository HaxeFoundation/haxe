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
import php7.db.PDO;
import sys.db.*;

@:coreApi class Sqlite {
	public static function open( file:String ) : Connection {
		return new SQLiteConnection(file);
	}
}

private class SQLiteConnection implements Connection {
	var db:PDO;

	public function new( file:String ) {
		db = new PDO('sqlite:$file');
		db.setAttribute(PDO.ATTR_ERRMODE, PDO.ERRMODE_EXCEPTION);
	}

	public function request( s : String ) : ResultSet {
		var result = db.query(s);
		return new SQLiteResultSet(result);
	}

	public function close() : Void {
		Global.unset(db);
	}

	public function escape( s : String ) : String {
		return db.quote(s);
	}

	public function quote( s : String ) : String {
		if( s.indexOf("\000") >= 0 ) {
			return "x'" + base16_encode(s) + "'";
		}
		return db.quote(s);
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
		return Syntax.int(db.lastInsertId());
	}

	public function dbName() : String {
		return 'SQLite';
	}

	public function startTransaction() : Void {
		db.beginTransaction();
	}

	public function commit() : Void {
		db.commit();
	}

	public function rollback() : Void {
		db.rollBack();
	}

	function base16_encode(str : String) {
		str = untyped __call__("unpack", "H"+(2 * str.length), str);
		str = untyped __call__("chunk_split", (str:NativeString)[1]);
		return str;
	}
}

private class SQLiteResultSet implements ResultSet {
	static var hxAnonClassName = Boot.getHxAnon().phpClassName;

	public var length(get,null) : Int;
	public var nfields(get,null) : Int;

	var result:PDOStatement;
	var fetchedRow:NativeArray;

	public function new( result:PDOStatement ) {
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
		var rows = result.fetchAll(PDO.FETCH_CLASS, hxAnonClassName);
		Syntax.foreach(rows, function(_, row) list.add(row));
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
		return [for (i in 0...result.columnCount()) result.getColumnMeta(i)['name']];
	}

	function fetchNext() {
		var next:Dynamic = result.fetch(PDO.FETCH_ASSOC);
		fetchedRow = (next == false ? fetchedRow = null : next);
	}

	function withdrawFetched() : Dynamic {
		if (fetchedRow == null) return null;
		return Boot.createAnon(fetchedRow);
	}

	function get_length() return result.rowCount();
	function get_nfields() return result.columnCount();
}