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
package php.db;

import php.NativeArray;

import sys.db.Connection;
import sys.db.ResultSet;

import php.Lib;

/**
 * PDO::FETCH_COLUMN = 7
 * PDO::FETCH_CLASS = 8
 * PDO::FETCH_INTO = 9
 * PDO::PARAM_STR = 2
 * PDO::FETCH_BOTH = 4
 * PDO::FETCH_ORI_NEXT = 0
 */
class PDO
{
	public static function open(dsn : String, ?user : String, ?password : String, ?options : Dynamic) : Connection {
		return new PDOConnection(dsn, user, password, options);
	}
}

extern class PDOClass
{
//	public function new(dns : String, ?username : String, ?password : String, ?driver_options : NativeArray) : Void;
	public function beginTransaction() : Bool;
	public function commit() : Bool;
	public function errorCode() : Dynamic;
	public function errorInfo() : NativeArray;
	public function exec(statement : String) : Int;
	public function getAttribute(attribute : Int) : Dynamic;
	public function getAvailableDrivers() : NativeArray;
	public function lastInsertId(?name : String) : String;
	public function prepare(statement : String, driver_options : NativeArray) : PDOStatement;
	public function query(statement : String, mode : Int) : PDOStatement;
	public function quote(String : String, ?parameter_type : Int = 2) : String;
	public function rollBack() : Bool;
	public function setAttribute(attribute : Int, value : Dynamic) : Bool;
}

extern class PDOStatement
{
	public function bindColumn(column : Dynamic, param : Dynamic, ?type : Int, ?maxlen : Int, ?driverdata : Dynamic) : Bool;
	public function bindParam(parameter : Dynamic, variable : Dynamic, ?data_type : Int, ?length : Int, ?driver_options : Dynamic) : Bool;
	public function bindValue(parameter : Dynamic, value : Dynamic, ?data_type : Int) : Bool;
	public function closeCursor() : Bool;
	public function columnCount() : Int;
	public function debugDumpParams() : Bool;
	public function errorCode() : String;
	public function errorInfo() : NativeArray;
	public function execute(input_parameters : NativeArray) : Bool;
	public function fetch(?fetch_style : Int = 4, ?cursor_orientation : Int = 0, ?cursor_offset : Int = 0) : Dynamic;
	public function fetchAll(?fetch_style : Int) : NativeArray;
	public function fetchColumn(?column_number : Int = 0) : String;
	public function fetchObject(?class_name : String, ?ctor_args : NativeArray) : Dynamic;
	public function getAttribute(attribute : Int) : Dynamic;
	public function getColumnMeta(column : Int) : NativeArray;
	public function nextRowset() : Bool;
	public function rowCount() : Int;
	public function setAttribute(attribute : Int, value : Dynamic) : Bool;
	public function setFetchMode(mode : Int, ?fetch : Dynamic, ?ctorargs : NativeArray) : Bool;
}

private class PDOConnection implements Connection {

	var pdo : PDOClass;
	var dbname : String;

	public function new(dsn : String, ?user : String, ?password : String, ?options : Dynamic) {
		if(null == options)
			pdo = untyped __call__("new PDO", dsn, user, password);
		else
		{
			var arr : NativeArray = untyped __call__("array");
			for (key in Reflect.fields(options))
				arr[untyped key] = Reflect.field(options, key);
			pdo = untyped __call__("new PDO", dsn, user, password, arr);
		}
		dbname = dsn.split(':').shift();
		switch(dbname.toLowerCase())
		{
			case "sqlite":
				dbname = "SQLite";
			case "mysql":
				dbname = "MySQL";
		}
	}

	public function close() {
		pdo = null;
		untyped __call__("unset", pdo);
	}

	public function request( s : String ) : ResultSet {
		var result = pdo.query(s, untyped __php__("PDO::PARAM_STR"));
		if(untyped __physeq__(result, false))
		{
			var info = Lib.toHaxeArray(pdo.errorInfo());
			throw "Error while executing " + s + " (" + info[2] + ")";
		}
		var db = dbname.toLowerCase();
		switch(db)
		{
			case "sqlite":
				return new AllResultSet(result, new DBNativeStrategy(db));
			default: // mysql
				return new PDOResultSet(result, new PHPNativeStrategy());
		}
	}

	public function escape( s : String ) {
		var output = pdo.quote(s);
		return output.length > 2 ? output.substr(1, output.length-2) : output;
	}

	public function quote( s : String ) {
		if( s.indexOf("\000") >= 0 )
			return "x'"+untyped __call__('bin2hex', s)+"'";
		return pdo.quote(s);
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
		return cast(Std.parseInt(pdo.lastInsertId()), Int);
	}

	public function dbName() {
		return dbname;
	}

	public function startTransaction() {
		pdo.beginTransaction();
	}

	public function commit() {
		pdo.commit();
	}

	public function rollback() {
		pdo.rollBack();
	}
}

private class TypeStrategy {
	public function new() {
	}
	public function map(data : NativeArray) : Dynamic
	{
		return throw "must override";
	}

	public static function convert(v : String, type : String) : Dynamic {
		if (v == null) return v;
		switch(type) {
			case "bool":
				return untyped __call__("(bool)", v);
			case "int":
				return untyped __call__("intval", v);
			case "float":
				return untyped __call__("floatval", v);
			case "date":
				return Date.fromString(v);
			case "blob":
				return haxe.io.Bytes.ofString(v);
			default:
				return v;
		}
	}
}

private class PHPNativeStrategy extends TypeStrategy {
	static inline var KEY = "native_type";
	override function map(data : NativeArray) : Dynamic {
		if (!untyped __call__("isset", data[KEY])) {
			if (untyped __call__("isset", data["precision"])) {
//				if (untyped __call__("isset", data["len"]) && data["len"] == 1)
//					return "bool"
//				else
					return "int";
			} else
				return "string";
		}
		var pdo_type_str:Int = untyped __php__("PDO::PARAM_STR");
		var pdo_type : Int = untyped data["pdo_type"];

		var type : String = untyped data[KEY];
		type = type.toLowerCase();
		switch(type)
		{
			case "float", "decimal", "double", "newdecimal":
				return "float";
			case "date", "datetime", "timestamp":
				return "date";
			case "bool", "tinyint(1)":
				return "bool";
			case "int", "int24", "int32", "long", "longlong", "short", "tiny":
				return "int";
			case "blob" if (pdo_type == pdo_type_str):
			 	return "string";
			case "blob":
				return "blob";
			default:
				return "string";
		}
	}
}

private class DBNativeStrategy extends PHPNativeStrategy {
	static inline var SUFFIX = ":decl_type";
	var dbname : String;
	var key : String;
	public function new(dbname : String)
	{
		super();
		this.dbname = dbname.toLowerCase();
		this.key = dbname + SUFFIX;
	}

	override function map(data : NativeArray) : Dynamic {
		if (!untyped __call__("isset", data[key]))
			return super.map(data);
		var type : String = untyped data[key];
		type = type.toLowerCase();
		switch(type)
		{
			case "real":
				return "float";
			case "integer":
				return "int";
			default:
				return "string";
		}
	}
}

private class BaseResultSet implements ResultSet {
	var pdo : PDOStatement;
	var typeStrategy : TypeStrategy;
	var _fields : Int;
	var _columnNames : Array<String>;
	var _columnTypes : Array<String>;

	public var length(get, null) : Int;
	public var nfields(get, null) : Int;

	public function new(pdo : PDOStatement, typeStrategy : TypeStrategy)
	{
		this.pdo = pdo;
		this.typeStrategy = typeStrategy;
		this._fields = pdo.columnCount();
		this._columnNames = [];
		this._columnTypes = [];
		feedColumns();
	}

	private function feedColumns() {
		for (i in 0..._fields) {
			var data = pdo.getColumnMeta(i);
			_columnNames.push(data[untyped 'name']);
			_columnTypes.push(typeStrategy.map(data));
		}
	}

	public function getFloatResult(index : Int) : Float {
		return untyped __call__("floatval", getResult(index));
	}

	public function getIntResult(index : Int) : Int {
		return untyped __call__("intval", getResult(index));
	}

	public function getResult(index : Int) : String {
		return throw "must override";
	}

	public function hasNext() : Bool {
		return throw "must override";
	}

	function get_length() : Int {
		return throw "must override";
	}

	function nextRow() : NativeArray {
		return throw "must override";
	}

	public function next() : Dynamic {
		var row = nextRow();
		var o : Dynamic = { };
		for (i in 0..._fields)
			Reflect.setField(o, _columnNames[i], TypeStrategy.convert(row[i], _columnTypes[i]));
		return o;
	}

	function get_nfields() : Int {
		return _fields;
	}

	public function results() : List<Dynamic>
	{
		var list = new List();
		while (hasNext())
			list.add(next());
		return list;
	}

	public function getFieldsNames() : Array<String> {
		return throw "Not implemented";
	}
}

private class AllResultSet extends BaseResultSet {
	var all : NativeArray;
	var pos : Int;
	var _length : Int;

	public function new(pdo : PDOStatement, typeStrategy : TypeStrategy)
	{
		super(pdo, typeStrategy);
		this.all = pdo.fetchAll(untyped __php__("PDO::FETCH_NUM"));
		this.pos = 0;
		this._length = untyped __call__("count", all);
	}

	override function getResult(index : Int) : String {
		untyped if(__call__("isset", all[0]) && __call__("isset", all[0][index]))
			return all[0][index];
		else
			return null;
	}

	override function hasNext() : Bool {
		return pos < _length;
	}

	override function get_length() : Int {
		return _length;
	}

	override function nextRow() : NativeArray {
		return all[pos++];
	}
}

private class PDOResultSet extends BaseResultSet {
	private var cache : NativeArray;
	public function new(pdo : PDOStatement, typeStrategy : TypeStrategy)
	{
		super(pdo, typeStrategy);
	}

	override function getResult(index : Int) : String {
		if (!hasNext())
			return null;
		return cache[index];
	}

	override function hasNext() {
		if(untyped __physeq__(null, cache))
			cacheRow();
		return (untyped cache);
	}

	override function get_length() {
		if (untyped __physeq__(pdo, false))
			return 0;
		return pdo.rowCount();
	}

	private function cacheRow() {
		cache = untyped pdo.fetch(__php__("PDO::FETCH_NUM"), __php__("PDO::FETCH_ORI_NEXT"));
	}

	override function nextRow()
	{
		if (!hasNext())
			return null;
		else {
			var v = cache;
			cache = null;
			return v;
		}
	}
}
