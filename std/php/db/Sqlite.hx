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
package php.db;

import php.db.Connection;

private class SqliteConnection implements Connection {

	var c : Void;
	var e : Void;

	public function new( file : String ) {
		c = untyped __call__("sqlite_open", file, 0666, e);
	}

	public function close() {
		untyped __call__("sqlite_close", c);
		untyped __call__("unset", c);
	}

	public function request( s : String ) : ResultSet {
		var h = untyped __call__("sqlite_query", c, s, __php__("SQLITE_BOTH"), e);
		if(untyped __physeq__(h, false))
			throw "Error while executing "+s+" ("+e+")";
		return new SqliteResultSet(cast h);
	}

	public function escape( s : String ) {
		return untyped __call__("sqlite_escape_string", s);
	}

	public function quote( s : String ) {
		if( s.indexOf("\000") >= 0 )
			return "x'"+base16_encode(s)+"'";
		return "'" + untyped __call__("sqlite_escape_string", s) + "'";
	}

	public function lastInsertId() {
		return untyped __call__("sqlite_last_insert_rowid", c);
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

	function base16_encode(str : String) {
		str = untyped __call__("unpack", "H"+(2 * str.length), str);
		str = untyped __call__("chunk_split", untyped str[1]);
		return str;
	}
}


private class SqliteResultSet implements ResultSet {

	public var length(getLength,null) : Int;
	public var nfields(getNFields,null) : Int;
	var r : Void;
	var cache : List<Dynamic>;

	public function new( r ) {
		cache = new List();
		this.r = r;
		hasNext(); // execute the request
	}

	function getLength() {
		if( nfields != 0 ) {
			while( true ) {
				var c = doNext();
				if( c == null )
					break;
				cache.add(c);
			}
			return cache.length;
		}
		return untyped __call__("sqlite_num_rows", r);
	}

	function getNFields() {
		return untyped __call__("sqlite_num_fields", r);
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
		var c = untyped __call__("sqlite_fetch_array", r, __php__("SQLITE_BOTH"));
		if(untyped __physeq__(c, false))
			return null;
		return php.Boot.__anonymous(c);
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

	public function getResult( n : Int ) : String {
		return Reflect.field(next(), cast n);
	}

	public function getIntResult( n : Int ) : Int {
		return untyped __call__("intval", Reflect.field(next(), cast n));
	}

	public function getFloatResult( n : Int ) : Float {
		return untyped __call__("floatval", Reflect.field(next(), cast n));
	}
}

class Sqlite {

	public static function open( file : String ) : Connection {
		return new SqliteConnection(file);
	}

}
