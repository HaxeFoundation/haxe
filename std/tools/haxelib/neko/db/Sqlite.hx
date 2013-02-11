/*
 * Copyright (c) 2005, The haXe Project Contributors
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE HAXE PROJECT CONTRIBUTORS "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE HAXE PROJECT CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 */
package neko.db;

import sys.db.Connection;
import sys.db.ResultSet;

private class SqliteConnection implements Connection {

	var c : Dynamic;

	public function new( file : String ) {
		c = _connect(untyped file.__s);
	}

	public function close() {
		_close(c);
	}

	public function request( s : String ) : ResultSet {
		try {
			return new SqliteResultSet(_request(c,untyped s.__s));
		} catch( e : String ) {
			throw "Error while executing "+s+" ("+e+")";
		}
	}

	public function escape( s : String ) {
		return s.split("'").join("''");
	}

	public function quote( s : String ) {
		if( s.indexOf("\000") >= 0 )
			return "x'"+new String(untyped _encode(s.__s,"0123456789ABCDEF".__s))+"'";
		return "'"+s.split("'").join("''")+"'";
	}

	public function addValue( s : StringBuf, v : Dynamic ) {
		var t = untyped __dollar__typeof(v);
		if( untyped (t == __dollar__tint || t == __dollar__tnull) )
			s.add(v);
		else if( untyped t == __dollar__tbool )
			s.add(if( v ) 1 else 0);
		else
			s.add(quote(Std.string(v)));
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

	static var _encode = neko.Lib.load("std","base_encode",2);
	static var _connect = neko.Lib.load("sqlite","connect",1);
	static var _close = neko.Lib.load("sqlite","close",1);
	static var _request = neko.Lib.load("sqlite","request",2);
	static var _last_id = neko.Lib.load("sqlite","last_insert_id",1);
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
				var c = doNext();
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
		return doNext();
	}

	private function doNext() : Dynamic {
		var c = result_next(r);
		if( c == null )
			return null;
		untyped {
			var f = __dollar__objfields(c);
			var i = 0;
			var l = __dollar__asize(f);
			while( i < l ) {
				var v = __dollar__objget(c,f[i]);
				if( __dollar__typeof(v) == __dollar__tstring )
					__dollar__objset(c,f[i],new String(v));
				i = i + 1;
			}
		}
		return c;
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

	static var result_next = neko.Lib.load("sqlite","result_next",1);
	static var result_get_length = neko.Lib.load("sqlite","result_get_length",1);
	static var result_get_nfields = neko.Lib.load("sqlite","result_get_nfields",1);
	static var result_get = neko.Lib.load("sqlite","result_get",2);
	static var result_get_int = neko.Lib.load("sqlite","result_get_int",2);
	static var result_get_float = neko.Lib.load("sqlite","result_get_float",2);

}

class Sqlite {

	public static function open( file : String ) : Connection {
		return new SqliteConnection(file);
	}

}
