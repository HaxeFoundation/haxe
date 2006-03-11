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

class Connection {

	private var __c : Void;

	private function new(c) {
		__c = c;
	}

	public function selectDB( db : String ) {
		sql_select_db(this.__c,untyped db.__s);
	}

	public function request( s : String ) : ResultSet {
		var r = sql_request(this.__c,untyped s.__s);
		untyped ResultSet.result_set_conv_date(r,function(d) { return Date.new1(d); });
		return untyped new ResultSet(r);
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

	private static var __use_date = Date;
	private static var sql_select_db = neko.Lib.load("mysql","select_db",2);
	private static var sql_request = neko.Lib.load("mysql","request",2);
	private static var sql_close = neko.Lib.load("mysql","close",1);
	private static var sql_escape = neko.Lib.load("mysql","escape",2);

}