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

import haxe.extern.*;
import php.*;

@:native('Mysqli_stmt')
extern class Mysqli_stmt {
	var affected_rows(default, null):Int;
	var errno(default, null):Int;
	var error_list(default, null):NativeArray;
	var error(default, null):String;
	var field_count(default, null):Int;
	var insert_id(default, null):Int;
	var num_rows(default, null):Int;
	var param_count(default, null):Int;
	var sqlstate(default, null):String;

	function new(link:Mysqli, ?query:String):Void;
	function attr_get(attr:Int):Int;
	function attr_set(attr:Int, mode:Int):Bool;
	function bind_param(types:String, var1:Ref<Dynamic>, args:Rest<Ref<Dynamic>>):Bool;
	function bind_result(var1:Ref<Dynamic>, args:Rest<Ref<Dynamic>>):Bool;
	function close():Bool;
	function data_seek(offset:Int):Void;
	function execute():Bool;
	function fetch():Bool;
	function free_result():Void;
	function get_result():Mysqli_result;
	function get_warnings(stmt:Mysqli_stmt):{};
	function prepare(query:String):Bool;
	function reset():Bool;
	function result_metadata():Mysqli_result;
	function send_long_data(param_nr:Int, data:String):Bool;
	function store_result():Bool;
}
