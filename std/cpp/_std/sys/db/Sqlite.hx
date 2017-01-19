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
      {
         var hexChars = new Array<String>();
         for(i in 0...s.length)
           hexChars.push( StringTools.hex( StringTools.fastCodeAt(s,i),2 ) );
			return "x'"+ hexChars.join("") +"'";
      }
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

	public function lastInsertId() : Int{
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


   @:extern @:native("_hx_sqlite_connect")
	public static function _connect(filename:String):Dynamic return null;
   @:extern @:native("_hx_sqlite_request")
	public static function _request(handle:Dynamic,req:String):Dynamic return null;
   @:extern @:native("_hx_sqlite_close")
	public static function _close(handle:Dynamic):Void { };
   @:extern @:native("_hx_sqlite_last_insert_id")
	public static function _last_id(handle:Dynamic):Int return 0;

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



   @:extern @:native("_hx_sqlite_result_next")
	public static function result_next(handle:Dynamic):Dynamic return null;
   @:extern @:native("_hx_sqlite_result_get_length")
	public static function result_get_length(handle:Dynamic):Int return 0;
   @:extern @:native("_hx_sqlite_result_get_nfields")
	public static function result_get_nfields(handle:Dynamic):Int return 0;
   @:extern @:native("_hx_sqlite_result_get")
	public static function result_get(handle:Dynamic,i:Int) : String return null;
   @:extern @:native("_hx_sqlite_result_get_int")
	public static function result_get_int(handle:Dynamic,i:Int) : Int return 0;
   @:extern @:native("_hx_sqlite_result_get_float")
	public static function result_get_float(handle:Dynamic,i:Int):Float return 0.0;

}

@:buildXml('<include name="${HXCPP}/src/hx/libs/sqlite/Build.xml"/>')
@:coreApi class Sqlite {

	public static function open( file : String ) : Connection {
		return new SqliteConnection(file);
	}

}
