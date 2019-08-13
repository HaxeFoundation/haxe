/*
 * Copyright (C)2005-2019 Haxe Foundation
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

private class MysqlParams {
	public var host:hl.Bytes;
	public var user:hl.Bytes;
	public var pass:hl.Bytes;
	public var socket:hl.Bytes;
	public var port:Int;

	public function new() {}
}

private typedef ConnectionHandler = hl.Abstract<"mysql_cnx">;
private typedef ResultHandler = hl.Abstract<"mysql_result">;

@:hlNative("mysql")
private class MysqlResultSet implements sys.db.ResultSet {
	public var length(get, null):Int;
	public var nfields(get, null):Int;

	private var r:ResultHandler;
	private var cache:Dynamic;

	function new(r) {
		this.r = r;
	}

	private function get_length() {
		return result_get_length(r);
	}

	private function get_nfields() {
		return result_get_nfields(r);
	}

	public function hasNext() {
		if (cache == null)
			cache = next();
		return (cache != null);
	}

	public function next():Dynamic {
		var c = cache;
		if (c != null) {
			cache = null;
			return c;
		}
		c = result_next(r);
		return c;
	}

	public function results():List<Dynamic> {
		var l = new List();
		while (hasNext())
			l.add(next());
		return l;
	}

	public function getResult(n:Int) {
		var v = result_get(r, n);
		if (v == null)
			return null;
		return @:privateAccess String.fromUTF8(v);
	}

	public function getIntResult(n:Int):Int {
		return result_get_int(r, n);
	}

	public function getFloatResult(n:Int):Float {
		return result_get_float(r, n);
	}

	public function getFieldsNames():Array<String> {
		var a = result_get_fields_names(r);
		return [for (v in a) @:privateAccess String.fromUTF8(v)];
	}

	static function result_get_length(r:ResultHandler):Int {
		return 0;
	}

	static function result_get_nfields(r:ResultHandler):Int {
		return 0;
	}

	static function result_next(r:ResultHandler):Dynamic {
		return null;
	}

	static function result_get(r:ResultHandler, n:Int):hl.Bytes {
		return null;
	}

	static function result_get_int(r:ResultHandler, n:Int):Int {
		return 0;
	}

	static function result_get_float(r:ResultHandler, n:Int):Float {
		return 0.;
	}

	static function result_get_fields_names(r:ResultHandler):hl.NativeArray<hl.Bytes> {
		return null;
	}
}

@:hlNative("mysql")
private class MysqlConnection implements Connection {
	var h:ConnectionHandler;

	function new(h) {
		this.h = h;
	}

	public function close() {
		if (h != null)
			close_wrap(h);
		h = null;
	}

	public function request(s:String) @:privateAccess {
		var len = 0;
		var b = s.bytes.utf16ToUtf8(0, len);
		return new MysqlResultSet(request_wrap(h, b, len));
	}

	public function escape(s:String) @:privateAccess {
		var len = 0;
		var utf = s.bytes.utf16ToUtf8(0, len);
		return String.fromUTF8(escape_wrap(h, utf, len));
	}

	public function quote(s:String) {
		return "'" + escape(s) + "'";
	}

	public function addValue(s:StringBuf, v:Dynamic) {
		if (v == null) {
			s.add(null);
			return;
		}
		var t = hl.Type.getDynamic(v).kind;
		if (t == HI32 || t == HF64)
			s.add(v);
		else if (t == HBool)
			s.addChar(if (v) "1".code else "0".code);
		else {
			s.addChar("'".code);
			s.add(escape(Std.string(v)));
			s.addChar("'".code);
		}
	}

	public function lastInsertId() {
		return request("SELECT LAST_INSERT_ID()").getIntResult(0);
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

	static function close_wrap(h:ConnectionHandler) {}

	static function connect_wrap(p:MysqlParams):ConnectionHandler {
		return null;
	}

	static function select_db_wrap(h:ConnectionHandler, db:hl.Bytes):Bool {
		return false;
	}

	@:hlNative("mysql", "request")
	static function request_wrap(h:ConnectionHandler, rq:hl.Bytes, rqLen:Int):ResultHandler {
		return null;
	}

	@:hlNative("mysql", "escape")
	static function escape_wrap(h:ConnectionHandler, str:hl.Bytes, len:Int):hl.Bytes {
		return null;
	}

	static function setConvFuns(fstring:Dynamic, fbytes:Dynamic, fdate:Dynamic, fjson:Dynamic) {};
}

class Mysql {
	static var INIT_DONE = false;

	public static function connect(params:{
		host:String,
		?port:Int,
		user:String,
		pass:String,
		?socket:String,
		?database:String
	}):sys.db.Connection@:privateAccess {
		if (!INIT_DONE) {
			INIT_DONE = true;
			MysqlConnection.setConvFuns(function(v:hl.Bytes) return @:privateAccess String.fromUTF8(v),
			function(v:hl.Bytes, len:Int) return new haxe.io.Bytes(v, len), function(t) return Date.fromTime(1000. * t),
			function(v:hl.Bytes) return haxe.Json.parse(@:privateAccess String.fromUTF8(v)));
		}
		var p = new MysqlParams();
		p.host = params.host == null ? null : params.host.toUtf8();
		p.user = params.user.toUtf8();
		p.pass = params.pass.toUtf8();
		p.socket = params.socket == null ? null : params.socket.toUtf8();
		p.port = params.port == null ? 3306 : params.port;
		var cnx = new MysqlConnection(MysqlConnection.connect_wrap(p));
		if (params.database != null && !MysqlConnection.select_db_wrap(cnx.h, params.database.toUtf8())) {
			cnx.close();
			throw "Failed to select database " + params.database;
		}
		return cnx;
	}
}
