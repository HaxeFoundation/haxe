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

private class MysqlResultSet implements ResultSet {

	public var length(getLength,null) : Int;
	public var nfields(getNFields,null) : Int;
	private var __r : Void;
	private var cache : Dynamic;

	public function new(r) {
		__r = r;
	}

	private function getLength() {
		return result_get_length(__r);
	}

	private function getNFields() {
		return result_get_nfields(__r);
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
		c = result_next(__r);
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
		return new String(result_get(__r,n));
	}

	public function getIntResult( n : Int ) : Int {
		return result_get_int(__r,n);
	}

	public function getFloatResult( n : Int ) : Float {
		return result_get_float(__r,n);
	}

	private static var result_get_length = neko.Lib.load("mysql","result_get_length",1);
	private static var result_get_nfields = neko.Lib.load("mysql","result_get_nfields",1);
	private static var result_next = neko.Lib.load("mysql","result_next",1);
	private static var result_get = neko.Lib.load("mysql","result_get",2);
	private static var result_get_int = neko.Lib.load("mysql","result_get_int",2);
	private static var result_get_float = neko.Lib.load("mysql","result_get_float",2);
	public static var result_set_conv_date = neko.Lib.load("mysql","result_set_conv_date",2);

}

private class MysqlConnection implements Connection {

	private var __c : Void;

	public function new(c) {
		__c = c;
	}

	public function selectDB( db : String ) {
		sql_select_db(this.__c,untyped db.__s);
	}

	public function request( s : String ) : ResultSet {
		try {
			var r = sql_request(this.__c,untyped s.__s);
			MysqlResultSet.result_set_conv_date(r,function(d) { return untyped Date.new1(d); });
			return new MysqlResultSet(r);
		} catch( e : Dynamic ) {
			untyped if( __dollar__typeof(e) == __dollar__tobject && __dollar__typeof(e.msg) == __dollar__tstring )
				e.msg = new String(e.msg);
			untyped __dollar__rethrow(e);
			return null;
		}
	}

	public function close() {
		sql_close(__c);
	}

	public function escape( s : String ) {
		return new String(sql_escape(__c,untyped s.__s));
	}

	public function quote( s : String ) {
		return "'"+escape(s)+"'";
	}

	public function lastInsertId() {
		return request("SELECT LAST_INSERT_ID()").getIntResult(0);
	}

	public function hasFeature( f ) {
		switch( f ) {
		case ForUpdate: return true;
		}
		return false;
	}

	private static var __use_date = Date;
	private static var sql_select_db = neko.Lib.load("mysql","select_db",2);
	private static var sql_request = neko.Lib.load("mysql","request",2);
	private static var sql_close = neko.Lib.load("mysql","close",1);
	private static var sql_escape = neko.Lib.load("mysql","escape",2);
}

class Mysql {

	public static function connect( params : {
		host : String,
		port : Int,
		user : String,
		pass : String,
		socket : String
	} ) : neko.db.Connection {
		var o = untyped {
			host : params.host.__s,
			port : params.port,
			user : params.user.__s,
			pass : params.pass.__s,
			socket : if( params.socket == null ) null else params.socket.__s
		};
		return new MysqlConnection(sql_connect(o));
	}

	static var sql_connect = neko.Lib.load("mysql","connect",1);

}