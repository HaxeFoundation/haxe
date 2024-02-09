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

package python.lib;

import python.KwArgs;
import python.Dict;
import python.lib.json.JSONEncoder;
import python.Tuple.Tuple2;

typedef JsonDumpsOptions = {
	var ?skipkeys:Bool;
	var ?ensure_ascii:Bool;
	var ?check_circular:Bool;
	var ?allow_nan:Bool;
	var ?cls:Dynamic;
	var ?indent:String;
	var ?separators:Tuple2<String, String>;
	@:native("default") var ?def:Dynamic->String;
	var ?sort_keys:Bool;
}

typedef JsonLoadsOptions = {
	var ?encoding:String;
	var ?cls:Dynamic;
	var ?object_hook:Dict<String, Dynamic>->Dynamic;
}

@:pythonImport("json")
extern class Json {
	static function loads(s:String, ?options:KwArgs<JsonLoadsOptions>):Dict<String, Dynamic>;
	static function dumps(x:Dynamic, ?options:KwArgs<JsonDumpsOptions>):String;
}
