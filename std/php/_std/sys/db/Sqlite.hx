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
package sys.db;

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

	public function addValue( s : StringBuf, v : Dynamic ) {
		if( untyped __call__("is_int", v) || __call__("is_null", v))
			s.add(v);
		else if( untyped __call__("is_bool", v) )
			s.add(if( v ) 1 else 0);
		else
			s.add(quote(Std.string(v)));
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
	var cache : Dynamic;

	public function new( r ) {
		this.r = r;
	}

	private function getLength() {
		if(untyped __physeq__(r, true))
			return untyped __call__("sqlite_changes", r);
		else if (untyped __physeq__(r, false))
			return 0;
		return untyped __call__("sqlite_num_rows", r);
	}

	private var _nfields : Int;
	private function getNFields() {
		if(_nfields == null)
			_nfields = untyped __call__("sqlite_num_fields", r);
		return _nfields;
	}

	public function hasNext() {
		if( cache == null )
			cache = next();
		return (cache != null);
	}

	private var cRow : ArrayAccess<String>;
	private function fetchRow() : Bool {
		cRow = untyped __call__("sqlite_fetch_array", r, __php__("SQLITE_ASSOC"));
		return ! untyped __physeq__(cRow, false);
	}

	public function next() : Dynamic {
		if( cache != null ) {
			var t = cache;
			cache = null;
			return t;
		}
		if(!fetchRow()) return null;
		return untyped __call__("_hx_anonymous", cRow);
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
		if(cRow == null && !fetchRow())
			return null;
		return untyped __call__("array_values", cRow)[n];
	}

	public function getIntResult( n : Int ) : Int {
		return untyped __call__("intval", getResult(n));
	}

	public function getFloatResult( n : Int ) : Float {
		return untyped __call__("floatval", getResult(n));
	}

	public function getFieldsNames() : Array<String> {
		return throw "Not implemented";
	}

}

@:coreApi class Sqlite {

	public static function open( file : String ) : Connection {
		return new SqliteConnection(file);
	}

}