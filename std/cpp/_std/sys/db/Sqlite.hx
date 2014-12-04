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
package sys.db;

private class SqliteConnection implements Connection {

	var c : Dynamic;

	public function new( file : String ) {
		c = _connect(file);
	}

	public function close() {
		_close(c);
	}

	public function request( s : String ) : ResultSet {
		try {
			return new SqliteResultSet(_request(c,s));
		} catch( e : String ) {
			throw "Error while executing "+s+" ("+e+")";
		}
	}

	public function escape( s : String ) {
		return s.split("'").join("''");
	}

	public function quote( s : String ) {
		if( s.indexOf("\000") >= 0 )
			return "x'"+new String(_encode(s,"0123456789ABCDEF"))+"'";
		return "'"+s.split("'").join("''")+"'";
	}

	public function addValue( s : StringBuf, v : Dynamic ) {
		if (v == null) {
			s.add(v);
      }
      else if (Std.is(v,Bool)) {
				s.add( v ? 1 : 0 );
			} else {
			var t:Int = untyped v.__GetType();
			if( t == 0xff )
				s.add(v);
			else if( t == 2 )
				s.add( untyped v.__GetInt() );
			else
				s.add(quote(Std.string(v)));
		}
	}

	public function lastInsertId() {
		return _last_id(c);
	}

	public function dbName() {
		return "SQLite";
	}

	public function startTransaction() {
		request("BEGIN TRANSACTION");
	}

	public function commit() {
		request("COMMIT");
		startTransaction(); // match mysql usage
	}

	public function rollback() {
		request("ROLLBACK");
		startTransaction(); // match mysql usage
	}

	static var _encode = cpp.Lib.load("std","base_encode",2);
	static var _connect = cpp.Lib.load("sqlite","sqlite_connect",1);
	static var _close = cpp.Lib.load("sqlite","close",1);
	static var _request = cpp.Lib.load("sqlite","request",2);
	static var _last_id = cpp.Lib.load("sqlite","last_insert_id",1);
}


private class SqliteResultSet implements ResultSet {

	public var length(get,null) : Int;
	public var nfields(get,null) : Int;
	var r : Dynamic;
	var cache : List<Dynamic>;

	public function new( r ) {
		cache = new List();
		this.r = r;
		hasNext(); // execute the request
	}

	function get_length() {
		if( nfields != 0 ) {
			while( true ) {
				var c = result_next(r);
				if( c == null )
					break;
				cache.add(c);
			}
			return cache.length;
		}
		return result_get_length(r);
	}

	function get_nfields() {
		return result_get_nfields(r);
	}

	public function hasNext() {
		var c = next();
		if( c == null )
			return false;
		cache.push(c);
		return true;
	}

	public function next() : Dynamic {
		var c = cache.pop();
		if( c != null )
			return c;
		return result_next(r);
	}

	public function results() : List<Dynamic> {
		var l = new List();
		while( true ) {
			var c = next();
			if( c == null )
				break;
			l.add(c);
		}
		return l;
	}

	public function getResult( n : Int ) {
		return new String(result_get(r,n));
	}

	public function getIntResult( n : Int ) : Int {
		return result_get_int(r,n);
	}

	public function getFloatResult( n : Int ) : Float {
		return result_get_float(r,n);
	}

	public function getFieldsNames() : Array<String> {
		return null;
	}

	static var result_next = cpp.Lib.load("sqlite","result_next",1);
	static var result_get_length = cpp.Lib.load("sqlite","result_get_length",1);
	static var result_get_nfields = cpp.Lib.load("sqlite","result_get_nfields",1);
	static var result_get = cpp.Lib.load("sqlite","result_get",2);
	static var result_get_int = cpp.Lib.load("sqlite","result_get_int",2);
	static var result_get_float = cpp.Lib.load("sqlite","result_get_float",2);

}

@:coreApi class Sqlite {

	public static function open( file : String ) : Connection {
		return new SqliteConnection(file);
	}

}
