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
class Json {

	public static inline function parse( text : String ) : Dynamic {
		#if !haxeJSON
		return phpJsonDecode(text);
		#else
		return haxe.format.JsonParser.parse(text);
		#end
	}

	public static inline function stringify( value : Dynamic, ?replacer:Dynamic -> Dynamic -> Dynamic, ?space:String ) : String {
		#if !haxeJSON
		return phpJsonEncode(value, replacer, space);
		#else
		return haxe.format.JsonPrinter.print(value, replacer, space);
		#end
	}

	static function phpJsonDecode(json:String):Dynamic {
		var val = untyped __call__("json_decode", json);
		if (val == null && untyped __php__("json_last_error() != JSON_ERROR_NONE")) {
			throw untyped __call__("json_last_error_msg");
		}
		return convertAfterDecode(val);
	}

	static function convertAfterDecode(val:Dynamic):Dynamic {
		var arr:php.NativeArray;
		if (untyped __call__("is_object", val)) {
			arr = phpMapArray(php.Lib.associativeArrayOfObject(val), convertAfterDecode);
			return untyped __call__("_hx_anonymous", arr);
		}
		else if (untyped __call__("is_array", val)) {
			arr = phpMapArray(val, convertAfterDecode);
			return php.Lib.toHaxeArray(arr);
		}
		else
			return val;
	}

	static function phpJsonEncode(val:Dynamic, ?replacer:Dynamic -> Dynamic -> Dynamic, ?space:String):String {
		if(null != replacer || null != space)
			return haxe.format.JsonPrinter.print(val, replacer, space);
		var json = untyped __call__("json_encode", convertBeforeEncode(val));
		if (untyped __physeq__(json, false))
			return throw "invalid json";
		else
			return json;
	}

	static function convertBeforeEncode(val:Dynamic):Dynamic {
		var arr:php.NativeArray;
		if (untyped __call__("is_object", val)) {
			switch (untyped __call__("get_class", val)) {
				case "_hx_anonymous", "stdClass" :
					arr = php.Lib.associativeArrayOfObject(val);
					if(untyped __php__('!{0}', arr)) return {};
				case "_hx_array" : arr = php.Lib.toPhpArray(val);
				case "Date" : return Std.string(val); //.split(" ").join("T"); //better with "T"?
				case "HList" : arr = php.Lib.toPhpArray(Lambda.array(val)); //convert List to array?
				case "_hx_enum" : return Type.enumIndex(val);
				case "StringMap", "IntMap" : arr = php.Lib.associativeArrayOfHash(val);
				default : arr = php.Lib.associativeArrayOfObject(val);
			}
		}
		else if (untyped __call__("is_array", val)) arr = val;
		else {
			if (untyped __call__("is_float",val) && !__call__("is_finite",val)) val = null;
			return val;
		}
		return phpMapArray(arr, convertBeforeEncode);
	}

	inline static function phpMapArray(arr:php.NativeArray, func:Dynamic->Dynamic):php.NativeArray {
		return untyped __call__("array_map", func, arr);
	}

}