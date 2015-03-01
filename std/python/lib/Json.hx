/*
 * Copyright (C)2005-2012 Haxe Foundation
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
import python.lib.Dict;
import python.lib.Tuple.Tup2;

@:pythonImport("json")
extern class Json {


	public static function loads (
		s:String,
		encoding:Null<String> = null,
		cls : Null<Dynamic> = null,
		object_hook:Null<Dict<String, Dynamic>->Dynamic> = null
		):Dict<String, Dynamic>;
	public static function dumps (x:Dynamic, skipkeys:Bool=false, ensure_ascii:Bool=true, check_circular:Bool=true,
		allow_nan:Bool=true,
		cls:Null<Dynamic> = null, indent:Null<String> = null,
		separators:Null<Tup2<String,String>>, /*default*/def:Null<Dynamic->String> = null, sort_keys:Bool=false, kw:KwArgs<Dynamic> = null):String;

}