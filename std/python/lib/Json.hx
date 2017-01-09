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
package python.lib;

import python.KwArgs;
import python.Dict;
import python.lib.json.JSONEncoder;
import python.Tuple.Tuple2;

typedef JsonDumpsOptions = {
	@:optional var skipkeys : Bool;
	@:optional var ensure_ascii : Bool;
	@:optional var check_circular : Bool;
	@:optional var allow_nan : Bool;
	@:optional var cls : Dynamic;
	@:optional var indent : String;
	@:optional var separators:Tuple2<String,String>;
	@:optional @:native("default") var def:Dynamic->String;
	@:optional var sort_keys:Bool;

}

typedef JsonLoadsOptions = {
	@:optional var encoding:String;
	@:optional var cls : Dynamic;
	@:optional var object_hook:Dict<String, Dynamic>->Dynamic;
}

@:pythonImport("json")
extern class Json {

	public static function loads ( s:String, ?options:KwArgs<JsonLoadsOptions>):Dict<String, Dynamic>;
	public static function dumps (x:Dynamic, ?options:KwArgs<JsonDumpsOptions>):String;

}