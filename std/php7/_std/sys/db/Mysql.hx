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

@:coreApi class Mysql {
	public static function connect(
		params : {
			host : String,
			?port : Int,
			user : String,
			pass : String,
			?socket : String,
			database : String
		}
	) : Connection {
		if (params.port == null) params.port = Std.parseInt(Global.ini_get('mysqli.default_port'));
		if (params.socket == null) params.socket = Global.ini_get('mysqli.default_socket');

		return new MysqlConnection(params.host, params.user, params.pass, params.database, params.port, params.socket);
	}
}

private class MysqlConnection extends Mysqli implements Connection {
	public function request( s : String ) : ResultSet {
		var result = query(s);
		if (result == false) throw 'Failed to perform db query';
		if (result == true) return null;

		return new MysqlResult(result);
	}

	public function close() : Void {
		close();
	}

	public function escape( s : String ) : String {
		return real_escape_string(s);
	}

	public function quote( s : String ) : String {
		return "'" + real_escape_string(s) + "'";
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
		return insert_id;
	}

	public function dbName() : String {
		var result = query('SELECT DATABASE()');
		if (!result) throw 'Failed to get database name';
		return result.fetch_row()[0];
	}

	public function startTransaction() : Void {
		var success = begin_transaction();
		if (!success) throw 'Failed to start transaction';
	}

	public function commit() : Void {
		var success = commit();
		if (!success) throw 'Failed to commit transaction';
	}

	public function rollback() : Void {
		var success = rollback();
		if (!success) throw 'Failed to rollback transaction';
	}

}

private class MysqlResultSet implements ResultSet {
	static var hxAnonClassName = Boot.getHxAnon().phpClassName;

	public var length(get,null) : Int;
	public var nfields(get,null) : Int;

	var result:Mysqli_result;
	var fetchedRow:NativeAssocArray<String>;

	public function new( result:Mysqli_result ) {
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
		var result = new List();

		result.data_seek(0);
		var row = result.fetch_object(hxAnonClassName);
		while (row != null) {
			list.add(row);
		}

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
		var fields = result.fetch_fields();
		return [for (field in fields) field.name];
	}

	function fetchNext() {
		nextRow = result.fetch_assoc();
	}

	function withdrawFetched() : Dynamic {
		if (fetchedRow == null) return null;
		return Boot.createAnon(fetchedRow);
	}

	function get_length() return result.num_rows;
	function get_nfields() return result.field_count;
}