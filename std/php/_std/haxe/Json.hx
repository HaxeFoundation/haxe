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

import php.*;
import haxe.format.JsonPrinter;

@:coreApi
class Json {

	public static inline function parse( text : String ) : Dynamic {
		#if haxeJSON
			return haxe.format.JsonParser.parse(text);
		#else
			return phpJsonDecode(text);
		#end
	}

	public static inline function stringify( value : Dynamic, ?replacer:Dynamic -> Dynamic -> Dynamic, ?space:String ) : String {
		#if haxeJSON
			return JsonPrinter.print(value, replacer, space);
		#else
			return phpJsonEncode(value, replacer, space);
		#end
	}

	static function phpJsonDecode(json:String):Dynamic {
		var value = Global.json_decode(json);
		if (value == null && Global.json_last_error() != Const.JSON_ERROR_NONE) {
			throw Global.json_last_error_msg();
		}
		return convertAfterDecode(value);
	}

	static function convertAfterDecode(value:Dynamic):Dynamic {
		if (Global.is_object(value)) {
			var result = new NativeAssocArray();
			var data = Syntax.array(value);
			Syntax.foreach(data, function(fieldName:String, fieldValue:Dynamic) {
				result[fieldName] = convertAfterDecode(fieldValue);
			});

			return Boot.createAnon(result);
		}

		if (Global.is_array(value)) {
			var result = new NativeIndexedArray();
			Syntax.foreach(value, function(index:Int, item:Dynamic) {
				result[index] = convertAfterDecode(item);
			});

			return @:privateAccess Array.wrap(result);
		}

		return value;
	}

	static function phpJsonEncode(value:Dynamic, ?replacer:Dynamic -> Dynamic -> Dynamic, ?space:String):String {
		if(null != replacer || null != space) {
			return JsonPrinter.print(value, replacer, space);
		}

		var json = Global.json_encode(convertBeforeEncode(value));
		if (Global.json_last_error() != Const.JSON_ERROR_NONE) {
			return throw Global.json_last_error_msg();
		}
		return json;
	}

	static function convertBeforeEncode(value:Dynamic):Dynamic {
		if (Std.is(value, Array)) {
			var result = new NativeIndexedArray();
			Syntax.foreach(value.arr, function(index:Int, item:Dynamic) {
				result[index] = convertBeforeEncode(item);
			});

			return result;
		}

		if (Global.is_object(value)) {
			var result = {};
			Syntax.foreach(value, function(fieldName:String, fieldValue:Dynamic) {
				Syntax.setField(result, fieldName, convertBeforeEncode(fieldValue));
			});

			return result;
		}

		if (Global.is_float(value) && !Global.is_finite(value)) {
			return null;
		}

		return value;
	}
}