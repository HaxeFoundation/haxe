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

package php.db;

import php.*;

@:native('PDO')
extern class PDO {
	@:phpClassConst static final PARAM_BOOL:Int;
	@:phpClassConst static final PARAM_NULL:Int;
	@:phpClassConst static final PARAM_INT:Int;
	@:phpClassConst static final PARAM_STR:Int;
	@:phpClassConst static final PARAM_LOB:Int;
	@:phpClassConst static final PARAM_STMT:Int;
	@:phpClassConst static final PARAM_INPUT_OUTPUT:Int;
	@:phpClassConst static final FETCH_LAZY:Int;
	@:phpClassConst static final FETCH_ASSOC:Int;
	@:phpClassConst static final FETCH_NAMED:Int;
	@:phpClassConst static final FETCH_NUM:Int;
	@:phpClassConst static final FETCH_BOTH:Int;
	@:phpClassConst static final FETCH_OBJ:Int;
	@:phpClassConst static final FETCH_BOUND:Int;
	@:phpClassConst static final FETCH_COLUMN:Int;
	@:phpClassConst static final FETCH_CLASS:Int;
	@:phpClassConst static final FETCH_INTO:Int;
	@:phpClassConst static final FETCH_FUNC:Int;
	@:phpClassConst static final FETCH_GROUP:Int;
	@:phpClassConst static final FETCH_UNIQUE:Int;
	@:phpClassConst static final FETCH_KEY_PAIR:Int;
	@:phpClassConst static final FETCH_CLASSTYPE:Int;
	@:phpClassConst static final FETCH_SERIALIZE:Int;
	@:phpClassConst static final FETCH_PROPS_LATE:Int;
	@:phpClassConst static final ATTR_AUTOCOMMIT:Int;
	@:phpClassConst static final ATTR_PREFETCH:Int;
	@:phpClassConst static final ATTR_TIMEOUT:Int;
	@:phpClassConst static final ATTR_ERRMODE:Int;
	@:phpClassConst static final ATTR_SERVER_VERSION:Int;
	@:phpClassConst static final ATTR_CLIENT_VERSION:Int;
	@:phpClassConst static final ATTR_SERVER_INFO:Int;
	@:phpClassConst static final ATTR_CONNECTION_STATUS:Int;
	@:phpClassConst static final ATTR_CASE:Int;
	@:phpClassConst static final ATTR_CURSOR_NAME:Int;
	@:phpClassConst static final ATTR_CURSOR:Int;
	@:phpClassConst static final ATTR_DRIVER_NAME:String;
	@:phpClassConst static final ATTR_ORACLE_NULLS:Int;
	@:phpClassConst static final ATTR_PERSISTENT:Int;
	@:phpClassConst static final ATTR_STATEMENT_CLASS:Int;
	@:phpClassConst static final ATTR_FETCH_TABLE_NAMES:Int;
	@:phpClassConst static final ATTR_STRINGIFY_FETCHES:Int;
	@:phpClassConst static final ATTR_EMULATE_PREPARES:Int;
	@:phpClassConst static final ERRMODE_SILENT:Int;
	@:phpClassConst static final ERRMODE_WARNING:Int;
	@:phpClassConst static final ERRMODE_EXCEPTION:Int;
	@:phpClassConst static final CASE_NATURAL:Int;
	@:phpClassConst static final CASE_LOWER:Int;
	@:phpClassConst static final CASE_UPPER:Int;
	@:phpClassConst static final NULL_NATURAL:Int;
	@:phpClassConst static final FETCH_ORI_PRIOR:Int;
	@:phpClassConst static final FETCH_ORI_FIRST:Int;
	@:phpClassConst static final FETCH_ORI_LAST:Int;
	@:phpClassConst static final FETCH_ORI_ABS:Int;
	@:phpClassConst static final FETCH_ORI_REL:Int;
	@:phpClassConst static final CURSOR_FWDONLY:Int;
	@:phpClassConst static final CURSOR_SCROLL:Int;
	@:phpClassConst static final ERR_NONE:String;
	@:phpClassConst static final PARAM_EVT_ALLOC:Int;
	@:phpClassConst static final PARAM_EVT_FREE:Int;
	@:phpClassConst static final PARAM_EVT_EXEC_PRE:Int;
	@:phpClassConst static final PARAM_EVT_EXEC_POST:Int;
	@:phpClassConst static final PARAM_EVT_FETCH_PRE:Int;
	@:phpClassConst static final PARAM_EVT_FETCH_POST:Int;
	@:phpClassConst static final PARAM_EVT_NORMALIZE:Int;

	function new(dns:String, ?username:String, ?password:String, ?options:NativeArray):Void;
	function beginTransaction():Bool;
	function commit():Bool;
	function errorCode():Dynamic;
	function errorInfo():NativeArray;
	function exec(statement:String):Int;
	function getAttribute(attribute:Int):Dynamic;
	function getAvailableDrivers():NativeArray;
	function lastInsertId(?name:String):String;
	function prepare(statement:String, ?driver_options:NativeArray):PDOStatement;
	function query(statement:String, ?mode:Int):PDOStatement;
	function quote(String:String, ?parameter_type:Int = 2):String;
	function rollBack():Bool;
	function setAttribute(attribute:Int, value:Dynamic):Bool;
}
