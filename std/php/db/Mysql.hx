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

private class MysqlConnection implements Connection {

	var c : Void;

	public function new( c : Void) {
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
		return new MysqlResultSet(cast h);
	}

	public function escape( s : String ) {
		return untyped __call__("mysql_real_escape_string", s, c);
	}

	public function quote( s : String ) {
		return "'" + untyped __call__("mysql_real_escape_string", s, c) + "'";
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
	public var length(getLength,null) : Int;
	public var nfields(getNFields,null) : Int;
	private var __r : Void;
	private var cache : Dynamic;

	public function new(r) {
		__r = r;
	}

	private function getLength() {
		return untyped __call__("mysql_num_rows", __r);
	}

	private function getNFields() {
		return untyped __call__("mysql_num_fields", __r);
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
		c = untyped __call__("mysql_fetch_array", __r);
		if(untyped __physeq__(c, false))
			return null;
		return php.Boot.__anonymous(c);
	}

	public function results() : List<Dynamic> {
		var l = new List();
		while( hasNext() )
			l.add(next());
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
		untyped __call__("mysql_select_db", params.database, c);
		return new MysqlConnection(c);
	}

}
