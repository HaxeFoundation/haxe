/*
 * Copyright (C)2005-2012 Haxe Foundation
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
package php.db;

import php.db.Connection;

private class MysqlConnection implements Connection {

	var c : Dynamic;

	public function new( c : Dynamic ) {
		this.c = c;
	}

	public function close() {
		untyped __call__("mysql_close", c);
		untyped __call__("unset", c);
	}

	public function request( s : String ) : ResultSet {
		var h = untyped __call__("mysql_query", s, c);
		if(untyped __physeq__(h, false))
			throw "Error while executing "+s+" ("+untyped __call__("mysql_error", c)+")";
		return new MysqlResultSet(cast h, cast c);
	}

	public function escape( s : String ) {
		return untyped __call__("mysql_real_escape_string", s, c);
	}

	public function quote( s : String ) {
		return "'" + untyped __call__("mysql_real_escape_string", s, c) + "'";
	}

	public function addValue( s : StringBuf, v : Dynamic ) {
		if( untyped __call__("is_int", v) || __call__("is_null", v))
			s.add(v);
		else if( untyped __call__("is_bool", v) )
			s.add(if( v ) 1 else 0);
		else
			s.add(quote(Std.string(v)));
	}

	public function lastInsertId() {
		return untyped __call__("mysql_insert_id", c);
	}

	public function dbName() {
		return "MySQL";
	}

	public function startTransaction() {
		request("START TRANSACTION");
	}

	public function commit() {
		request("COMMIT");
	}

	public function rollback() {
		request("ROLLBACK");
	}
}


private class MysqlResultSet implements ResultSet {
	public var length(get,null) : Int;
	public var nfields(get,null) : Int;
	private var __r : Dynamic;
	private var __c : Dynamic;
	private var cache : Dynamic;

	public function new(r, c) {
		__r = r;
		__c = c;
	}

	private function get_length() {
		if(untyped __physeq__(__r, true))
			return untyped __call__("mysql_affected_rows", __c);
		else if (untyped __physeq__(__r, false))
			return 0;
		return untyped __call__("mysql_num_rows", __r);
	}

	private var _nfields : Int;
	private function get_nfields() {
		if(_nfields == null)
			_nfields = untyped __call__("mysql_num_fields", __r);
		return _nfields;
	}

	private var _fieldsDesc : Array<Dynamic>;
	private function getFieldsDescription() {
		if(_fieldsDesc == null) {
			_fieldsDesc = [];
			for (i in 0...nfields) {
				var item = {
					name : untyped __call__("mysql_field_name", __r, i),
					type : untyped __call__("mysql_field_type", __r, i)
				};
				_fieldsDesc.push(item);
			}
		}
		return _fieldsDesc;
	}

	private function convert(v : Dynamic, type : String) : Dynamic {
		if (v == null) return v;
		switch(type) {
			case "int", "year":
				return untyped __call__("intval", v);
			case "real":
				return untyped __call__("floatval", v);
			case "datetime", "date":
				return Date.fromString(v);
			case "blob":
				return haxe.io.Bytes.ofData(v);
			default:
				return v;
		}
	}

	public function hasNext() {
		if( cache == null )
			cache = next();
		return (cache != null);
	}

	private var cRow : ArrayAccess<String>;
	private function fetchRow() : Bool {
		cRow = untyped __call__("mysql_fetch_array", __r, __php__("MYSQL_NUM"));
		return ! untyped __physeq__(cRow, false);
	}

	public function next() : Dynamic {
		if( cache != null ) {
			var t = cache;
			cache = null;
			return t;
		}
		if(!fetchRow()) return null;

		var o : Dynamic = {};
		var descriptions = getFieldsDescription();
		for(i in 0...nfields)
			Reflect.setField(o, descriptions[i].name, convert(cRow[i], descriptions[i].type));
		return o;
	}

	public function results() : List<Dynamic> {
		var l = new List();
		while( hasNext() )
			l.add(next());
		return l;
	}

	public function getResult( n : Int ) : String {
		if(cRow == null)
			if(!fetchRow())
				return null;
		return cRow[n];
	}

	public function getIntResult( n : Int ) : Int {
		return untyped __call__("intval", getResult(n));
	}

	public function getFloatResult( n : Int ) : Float {
		return untyped __call__("floatval", getResult(n));
	}

	public function getFieldsNames() : Array<String> {
		var fields = [];
		for( i in 0...nfields )
			fields.push(untyped __call__("mysql_field_name", __r, i));
		return fields;
	}

}

class Mysql {

	public static function connect( params : {
		host : String,
		port : Int,
		user : String,
		pass : String,
		socket : String,
		database : String
	} ) : php.db.Connection {
		var c = untyped __call__("mysql_connect",
			params.host + (params.port == null ? '' : ':'+params.port) + (params.socket == null ? '' : ':'+params.socket),
			params.user,
			params.pass);
		if(!untyped __call__("mysql_select_db", params.database, c))
			throw "Unable to connect to " + params.database;
		return new MysqlConnection(c);
	}

}
