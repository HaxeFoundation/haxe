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
#if !(core_api || cross || custom_target || eval)
#error "Please don't add haxe/std to your classpath, instead set HAXE_STD_PATH env var"
#end

/**
	The Std class provides standard methods for manipulating basic types.
**/
@:coreApi
class Std {
	@:deprecated('Std.is is deprecated. Use Std.isOfType instead.')
	extern static public function is(v:Dynamic, t:Dynamic):Bool;

	extern static public function isOfType(v:Dynamic, t:Dynamic):Bool;

	extern static public function downcast<T:{}, S:T>(value:T, c:Class<S>):S;

	@:deprecated('Std.instance() is deprecated. Use Std.downcast() instead.')
	extern static public function instance<T:{}, S:T>(value:T, c:Class<S>):S;

	static var toStringDepth = 0;

	static public function string(s:Dynamic):String {
		if (toStringDepth > haxe.runtime.Config.maxToStringDepth) {
			return "<...>";
		}
		++toStringDepth;
		try {
			var s = _string(s);
			--toStringDepth;
			return s;
		} catch (e:Dynamic) {
			--toStringDepth;
			throw e;
		}
	}

	extern static function _string(s:Dynamic):String;

	extern static public function int(x:Float):Int;

	extern static public function parseInt(x:String):Null<Int>;

	extern static public function parseFloat(x:String):Float;

	extern static public function random(x:Int):Int;
}
