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
import haxe.extern.*;

/**
	@see http://php.net/manual/en/class.mysqli-result.php
**/
@:native('Myslqi_result')
extern class Mysqli_result implements Traversable {
	var current_field(default, null):Int;
	var field_count(default, null):Int;
	var lengths(default, null):EitherType<Bool, NativeIndexedArray<Int>>;
	var num_rows(default, null):Int;

	function data_seek(offset:Int):Bool;
	function fetch_all(?resulttype:Int):NativeArray;
	function fetch_array(?resulttype:Int):NativeArray;
	function fetch_assoc():NativeAssocArray<String>;
	function fetch_field_direct(fieldnr:Int):MysqliFieldInfo;
	function fetch_field():MysqliFieldInfo;
	function fetch_fields():NativeIndexedArray<MysqliFieldInfo>;
	function fetch_object(?class_name:String = "stdClass", ?params:NativeArray):{};
	function fetch_row():NativeIndexedArray<String>;
	function field_seek(fieldnr:Int):Bool;
	function free():Void;
}

typedef MysqliFieldInfo = {
	name:String,
	orgname:String,
	table:String,
	orgtable:String,
	max_length:Int,
	length:Int,
	charsetnr:Int,
	flags:Int,
	type:Int,
	decimals:Int
}
