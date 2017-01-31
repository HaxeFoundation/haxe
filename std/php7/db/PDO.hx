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

import php.*;

@:native('PDO')
extern class PDO {
	@:phpClassConst static var PARAM_BOOL : Int;
	@:phpClassConst static var PARAM_NULL : Int;
	@:phpClassConst static var PARAM_INT : Int;
	@:phpClassConst static var PARAM_STR : Int;
	@:phpClassConst static var PARAM_LOB : Int;
	@:phpClassConst static var PARAM_STMT : Int;
	@:phpClassConst static var PARAM_INPUT_OUTPUT : Int;
	@:phpClassConst static var FETCH_LAZY : Int;
	@:phpClassConst static var FETCH_ASSOC : Int;
	@:phpClassConst static var FETCH_NAMED : Int;
	@:phpClassConst static var FETCH_NUM : Int;
	@:phpClassConst static var FETCH_BOTH : Int;
	@:phpClassConst static var FETCH_OBJ : Int;
	@:phpClassConst static var FETCH_BOUND : Int;
	@:phpClassConst static var FETCH_COLUMN : Int;
	@:phpClassConst static var FETCH_CLASS : Int;
	@:phpClassConst static var FETCH_INTO : Int;
	@:phpClassConst static var FETCH_FUNC : Int;
	@:phpClassConst static var FETCH_GROUP : Int;
	@:phpClassConst static var FETCH_UNIQUE : Int;
	@:phpClassConst static var FETCH_KEY_PAIR : Int;
	@:phpClassConst static var FETCH_CLASSTYPE : Int;
	@:phpClassConst static var FETCH_SERIALIZE : Int;
	@:phpClassConst static var FETCH_PROPS_LATE : Int;
	@:phpClassConst static var ATTR_AUTOCOMMIT : Int;
	@:phpClassConst static var ATTR_PREFETCH : Int;
	@:phpClassConst static var ATTR_TIMEOUT : Int;
	@:phpClassConst static var ATTR_ERRMODE : Int;
	@:phpClassConst static var ATTR_SERVER_VERSION : Int;
	@:phpClassConst static var ATTR_CLIENT_VERSION : Int;
	@:phpClassConst static var ATTR_SERVER_INFO : Int;
	@:phpClassConst static var ATTR_CONNECTION_STATUS : Int;
	@:phpClassConst static var ATTR_CASE : Int;
	@:phpClassConst static var ATTR_CURSOR_NAME : Int;
	@:phpClassConst static var ATTR_CURSOR : Int;
	@:phpClassConst static var ATTR_DRIVER_NAME : String;
	@:phpClassConst static var ATTR_ORACLE_NULLS : Int;
	@:phpClassConst static var ATTR_PERSISTENT : Int;
	@:phpClassConst static var ATTR_STATEMENT_CLASS : Int;
	@:phpClassConst static var ATTR_FETCH_TABLE_NAMES : Int;
	@:phpClassConst static var ATTR_STRINGIFY_FETCHES : Int;
	@:phpClassConst static var ATTR_EMULATE_PREPARES : Int;
	@:phpClassConst static var ERRMODE_SILENT : Int;
	@:phpClassConst static var ERRMODE_WARNING : Int;
	@:phpClassConst static var ERRMODE_EXCEPTION : Int;
	@:phpClassConst static var CASE_NATURAL : Int;
	@:phpClassConst static var CASE_LOWER : Int;
	@:phpClassConst static var CASE_UPPER : Int;
	@:phpClassConst static var NULL_NATURAL : Int;
	@:phpClassConst static var FETCH_ORI_PRIOR : Int;
	@:phpClassConst static var FETCH_ORI_FIRST : Int;
	@:phpClassConst static var FETCH_ORI_LAST : Int;
	@:phpClassConst static var FETCH_ORI_ABS : Int;
	@:phpClassConst static var FETCH_ORI_REL : Int;
	@:phpClassConst static var CURSOR_FWDONLY : Int;
	@:phpClassConst static var CURSOR_SCROLL : Int;
	@:phpClassConst static var ERR_NONE : String;
	@:phpClassConst static var PARAM_EVT_ALLOC : Int;
	@:phpClassConst static var PARAM_EVT_FREE : Int;
	@:phpClassConst static var PARAM_EVT_EXEC_PRE : Int;
	@:phpClassConst static var PARAM_EVT_EXEC_POST : Int;
	@:phpClassConst static var PARAM_EVT_FETCH_PRE : Int;
	@:phpClassConst static var PARAM_EVT_FETCH_POST : Int;
	@:phpClassConst static var PARAM_EVT_NORMALIZE : Int;

	function new( dns : String, ?username : String, ?password : String, ?options : NativeArray) : Void;
	function beginTransaction() : Bool;
	function commit() : Bool;
	function errorCode() : Dynamic;
	function errorInfo() : NativeArray;
	function exec(statement : String) : Int;
	function getAttribute(attribute : Int) : Dynamic;
	function getAvailableDrivers() : NativeArray;
	function lastInsertId(?name : String) : String;
	function prepare(statement : String, driver_options : NativeArray) : PDOStatement;
	function query(statement : String, ?mode : Int) : PDOStatement;
	function quote(String : String, ?parameter_type : Int = 2) : String;
	function rollBack() : Bool;
	function setAttribute(attribute : Int, value : Dynamic) : Bool;
}