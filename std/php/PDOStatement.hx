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
package php;

import php.NativeArray;

import php.Lib;

extern class PDOStatement
{
	public var queryString(default, null):String;
	
	@:overload(function(column : Dynamic, param : Dynamic):Bool { })
	@:overload(function(column : Dynamic, param : Dynamic, type : Int):Bool { })
	@:overload(function(column : Dynamic, param : Dynamic, type : Int, maxlen : Int):Bool { })
	public function bindColumn(column : Dynamic, param : Dynamic, type : Int, maxlen : Int, driverdata : Dynamic) : Bool;
	
	@:overload(function(parameter : Dynamic, variable : Dynamic, data_type : Int, length : Int):Bool{})
	@:overload(function(parameter : Dynamic, variable : Dynamic, data_type : Int, length : Int, driver_options : Dynamic):Bool{})
	public function bindParam(parameter : Dynamic, variable : Dynamic, ?data_type : Int = PDOClass.PARAM_STR) : Bool;
	
	public function bindValue(parameter : Dynamic, value : Dynamic, ?data_type : Int = PDOClass.PARAM_STR) : Bool;
	public function closeCursor() : Bool;
	public function columnCount() : Int;
	public function debugDumpParams() : Bool;
	public function errorCode() : String;
	public function errorInfo() : NativeArray;
	
	@:overload(function():Bool{})
	public function execute(input_parameters : NativeArray) : Bool;
	
	@:overload(function():Dynamic{})
	public function fetch(fetch_style : Int, ?cursor_orientation : Int = PDOClass.FETCH_ORI_NEXT, ?cursor_offset : Int = 0) : Dynamic;
	
	@:overload(function():Dynamic{})
	@:overload(function(fetch_style : Int):Dynamic{})
	@:overload(function(fetch_style : Int, fetch_argument:Dynamic):Dynamic{})
	public function fetchAll(fetch_style : Int, fetch_argument:Dynamic, ctor_args:NativeArray) : NativeArray;
	
	public function fetchColumn(?column_number : Int = 0) : String;
	
	@:overload(function(?class_name : String = "stdClass"): Dynamic{})
	public function fetchObject(?class_name : String = "stdClass", ctor_args : NativeArray) : Dynamic;
	
	public function getAttribute(attribute : Int) : Dynamic;
	public function getColumnMeta(column : Int) : NativeArray;
	public function nextRowset() : Bool;
	public function rowCount() : Int;
	public function setAttribute(attribute : Int, value : Dynamic) : Bool;
	
	@:overload(function(mode : Int) : Bool{})
	@:overload(function(mode : Int, fetch : Dynamic) : Bool{})
	public function setFetchMode(mode : Int, fetch : Dynamic, ctorargs : NativeArray) : Bool;
}