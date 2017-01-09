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
package haxe;

@:coreApi
#if (!haxeJSON && !old_browser)
@:native("JSON") extern
#end
class Json {

	#if haxeJSON inline #end
	public static function parse( text : String ) : Dynamic {
		return haxe.format.JsonParser.parse(text);
	}

	#if haxeJSON inline #end
	public static function stringify( value : Dynamic, ?replacer:Dynamic -> Dynamic -> Dynamic, ?space:String ) : String {
		return haxe.format.JsonPrinter.print(value, replacer, space);
	}

	#if (!haxeJSON && old_browser)
	static function __init__():Void untyped {
		if( __js__('typeof(JSON)') != 'undefined' )
			Json = __js__('JSON');
	}
	#end

}
