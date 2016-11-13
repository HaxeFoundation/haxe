/*
 * Copyright (C)2005-2016 Haxe Foundation
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
package php7.db;

import php7.*;
import sys.db.*;

@:native('PDO')
extern class PDO {
	@:phpClassConst static var FETCH_COLUMN : Int;
	@:phpClassConst static var FETCH_CLASS : Int;
	@:phpClassConst static var FETCH_INTO : Int;
	@:phpClassConst static var PARAM_STR : Int;
	@:phpClassConst static var FETCH_BOTH : Int;
	@:phpClassConst static var FETCH_ORI_NEXT : Int;

	public function new( dns : String, ?username : String, ?password : String, ?options : NativeArray) : Void;
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

@:native('PDOStatement')
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