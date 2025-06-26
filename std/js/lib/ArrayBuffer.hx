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

package js.lib;

@:native("ArrayBuffer")
extern class ArrayBuffer {
	static function isView(value:Dynamic):Bool;

	final byteLength:Int;

	/** @throws DOMError */
	function new(length:Int):Void;

	function slice(begin:Int, ?end:Int):ArrayBuffer;
}

#if (js_es <= 5)
@:ifFeature('js.lib.ArrayBuffer.slice')
private class ArrayBufferCompat {
	static function sliceImpl(begin, ?end) {
		var u = new js.lib.Uint8Array(js.Lib.nativeThis, begin, end == null ? null : (end - begin));
		var resultArray = new js.lib.Uint8Array(u.byteLength);
		resultArray.set(u);
		return resultArray.buffer;
	}

	static function __init__():Void
		untyped {
			// IE10 ArrayBuffer.slice polyfill
			if (js.Syntax.code("ArrayBuffer").prototype.slice == null)
				js.Syntax.code("ArrayBuffer").prototype.slice = sliceImpl;
		}
}
#end
