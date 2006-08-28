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

import neko.db.Connection;

private class SqliteConnection implements Connection {

	var c : Void;

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

	public function lastInsertId() {
		return _last_id(c);
	}

	public function hasFeature( f ) {
		switch( f ) {
		case ForUpdate: return false;
		}
		return false;
	}

	public function startTransaction() {
		request("BEGIN TRANSACTION");
	}

	public function commit() {
		request("COMMIT");
	}

	public function rollback() {
		request("ROLLBACK");
	}

	static var _encode = neko.Lib.load("std","base_encode",2);
	static var _connect = neko.Lib.load("sqlite","connect",1);
	static var _close = neko.Lib.load("sqlite","close",1);
	static var _request = neko.Lib.load("sqlite","request",2);
	static var _last_id = neko.Lib.load("sqlite","last_insert_id",1);
}


private class SqliteResultSet implements ResultSet {

	public var length(getLength,null) : Int;
	public var nfields(getNFields,null) : Int;
	var r : Void;
	var cache : Dynamic;

	public function new( r ) {
		this.r = r;
		hasNext(); // execute the request
	}

	function getLength() {
		return result_get_length(r);
	}

	function getNFields() {
		return result_get_nfields(r);
	}

	public function hasNext() {
		if( cache == null )
			cache = next();
		return (cache != null);
	}

	public function next() : Dynamic {
		var c = cache;
		if( c != null ) {
			cache = null;
			return c;
		}
		c = result_next(r);
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
		while( hasNext() )
			l.add(next());
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
