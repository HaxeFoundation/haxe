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

@:native('PDOStatement')
extern class PDOStatement {
	function bindColumn(column:Dynamic, param:Dynamic, ?type:Int, ?maxlen:Int, ?driverdata:Dynamic):Bool;
	function bindParam(parameter:Dynamic, variable:Dynamic, ?data_type:Int, ?length:Int, ?driver_options:Dynamic):Bool;
	function bindValue(parameter:Dynamic, value:Dynamic, ?data_type:Int):Bool;
	function closeCursor():Bool;
	function columnCount():Int;
	function debugDumpParams():Bool;
	function errorCode():String;
	function errorInfo():NativeArray;
	function execute(?input_parameters:NativeArray):Bool;
	function fetch(?fetch_style:Int = 4, ?cursor_orientation:Int = 0, ?cursor_offset:Int = 0):Dynamic;
	function fetchAll(?fetch_style:Int, ?fetch_argument:Dynamic, ?ctor_args:NativeArray):NativeArray;
	function fetchColumn(?column_number:Int = 0):String;
	function fetchObject(?class_name:String, ?ctor_args:NativeArray):Dynamic;
	function getAttribute(attribute:Int):Dynamic;
	function getColumnMeta(column:Int):NativeArray;
	function nextRowset():Bool;
	function rowCount():Int;
	function setAttribute(attribute:Int, value:Dynamic):Bool;
	function setFetchMode(mode:Int, ?fetch:Dynamic, ?ctorargs:NativeArray):Bool;
}
