package php.db;

import php.*;

@:native('PDOStatement')
extern class PDOStatement {
	function bindColumn(column : Dynamic, param : Dynamic, ?type : Int, ?maxlen : Int, ?driverdata : Dynamic) : Bool;
	function bindParam(parameter : Dynamic, variable : Dynamic, ?data_type : Int, ?length : Int, ?driver_options : Dynamic) : Bool;
	function bindValue(parameter : Dynamic, value : Dynamic, ?data_type : Int) : Bool;
	function closeCursor() : Bool;
	function columnCount() : Int;
	function debugDumpParams() : Bool;
	function errorCode() : String;
	function errorInfo() : NativeArray;
	function execute(input_parameters : NativeArray) : Bool;
	function fetch(?fetch_style : Int = 4, ?cursor_orientation : Int = 0, ?cursor_offset : Int = 0) : Dynamic;
	function fetchAll(?fetch_style : Int, ?fetch_argument:Dynamic, ?ctor_args:NativeArray) : NativeArray;
	function fetchColumn(?column_number : Int = 0) : String;
	function fetchObject(?class_name : String, ?ctor_args : NativeArray) : Dynamic;
	function getAttribute(attribute : Int) : Dynamic;
	function getColumnMeta(column : Int) : NativeArray;
	function nextRowset() : Bool;
	function rowCount() : Int;
	function setAttribute(attribute : Int, value : Dynamic) : Bool;
	function setFetchMode(mode : Int, ?fetch : Dynamic, ?ctorargs : NativeArray) : Bool;
}