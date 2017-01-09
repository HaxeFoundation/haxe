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

@:keep
private class D {

   @:extern @:native("_hx_mysql_connect")
	public static function connect(params:Dynamic):Dynamic return null;
   @:extern @:native("_hx_mysql_select_db")
	public static function select_db(handle:Dynamic, db:String):Void { }
   @:extern @:native("_hx_mysql_request")
	public static function request(handle:Dynamic,req:String):Dynamic return null;
   @:extern @:native("_hx_mysql_close")
	public static function close(handle:Dynamic):Dynamic return null;
   @:extern @:native("_hx_mysql_escape")
	public static function escape(handle:Dynamic,str:String):String return null;
   @:extern @:native("_hx_mysql_result_get_length")
	public static function result_get_length(handle:Dynamic):Int return 0;
   @:extern @:native("_hx_mysql_result_get_nfields")
	public static function result_get_nfields(handle:Dynamic):Int return 0;
   @:extern @:native("_hx_mysql_result_next")
	public static function result_next(handle:Dynamic):Dynamic return null;
   @:extern @:native("_hx_mysql_result_get")
	public static function result_get(handle:Dynamic,i:Int) : String return null;
   @:extern @:native("_hx_mysql_result_get_int")
	public static function result_get_int(handle:Dynamic,i:Int) : Int return 0;
   @:extern @:native("_hx_mysql_result_get_float")
	public static function result_get_float(handle:Dynamic,i:Int):Float return 0.0;
   @:extern @:native("_hx_mysql_result_get_fields_names")
	public static function result_fields_names(handle:Dynamic):Array<String> return null;

   @:extern @:native("_hx_mysql_set_conversion")
	public static function set_conv_funs(
      charsToBytes: cpp.Callable< Dynamic -> Dynamic >,
      intToDate: cpp.Callable< Float -> Dynamic > ) : Void {}

   public static function charsToBytes(data:Dynamic) : Dynamic
      return haxe.io.Bytes.ofData(data);

   public static function secondsToDate(seconds:Float) : Dynamic
      return Date.fromTime(seconds);

}

private class MysqlResultSet implements sys.db.ResultSet {

	public var length(get,null) : Int;
	public var nfields(get,null) : Int;
	private var __r : Dynamic;
	private var cache : Dynamic;

	public function new(r) {
		__r = r;
	}

	private function get_length() {
		return D.result_get_length(__r);
	}

	private function get_nfields() {
		return D.result_get_nfields(__r);
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
		c = D.result_next(__r);
		return c;
	}

	public function results() : List<Dynamic> {
		var l = new List();
		while( hasNext() )
			l.add(next());
		return l;
	}

	public function getResult( n : Int ) {
		return D.result_get(__r,n);
	}

	public function getIntResult( n : Int ) : Int {
		return D.result_get_int(__r,n);
	}

	public function getFloatResult( n : Int ) : Float {
		return D.result_get_float(__r,n);
	}

	public function getFieldsNames() : Array<String> {
		var a = D.result_fields_names(__r);
		return a;
	}

}

private class MysqlConnection implements sys.db.Connection {

	private var __c : Dynamic;

	public function new(c) {
		__c = c;
	 D.set_conv_funs( cpp.Function.fromStaticFunction(D.charsToBytes),
                     cpp.Function.fromStaticFunction(D.secondsToDate) );
    
	}

	public function request( s : String ) : sys.db.ResultSet {
			var r = D.request(this.__c, s);
			return new MysqlResultSet(r);
	}

	public function close() {
		D.close(__c);
	}

	public function escape( s : String ) {
		return D.escape(__c,s);
	}

	public function quote( s : String ) {
		return "'"+escape(s)+"'";
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
				s.add( untyped v.__GetInt() ? "1".code : "0".code );
			else {
				s.addChar("'".code);
				s.add(escape(Std.string(v)));
				s.addChar("'".code);
			}
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

	private static var __use_date = Date;
}

@:buildXml('<include name="${HXCPP}/src/hx/libs/mysql/Build.xml"/>')
@:coreApi class Mysql {

	public static function connect( params : {
		host : String,
		?port : Int,
		user : String,
		pass : String,
		?socket : String,
		database : String
	} ) : sys.db.Connection {
		var o = {
			host : params.host,
			port : if( params.port == null ) 3306 else params.port,
			user : params.user,
			pass : params.pass,
			socket : if( params.socket == null ) null else params.socket
		};
		var c = D.connect(o);
		try {
			D.select_db(c,params.database);
		} catch( e : Dynamic ) {
			D.close(c);
			cpp.Lib.rethrow(e);
		}
		return new MysqlConnection(c);
	}

}
