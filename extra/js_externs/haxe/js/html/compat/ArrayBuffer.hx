/*
 * Copyright (C)2005-2015 Haxe Foundation
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
package js.html.compat;

@:keep
class ArrayBuffer {

	public var byteLength : Int;
	var a : Array<Dynamic>;
	
	public function new( ?a : Dynamic ) {
		if( Std.is(a,Array) ) {
			this.a = a;
			byteLength = a.length;
		} else
			throw "TODO";
	}
	
	public function slice(begin,?end) {
		return new ArrayBuffer(a.slice(begin,end));
	}
	
	static function sliceImpl(begin,?end) {
		var u = new js.html.Uint8Array(untyped __js__('this'), begin, end == null ? null : end - begin);
        var result = new js.html.ArrayBuffer(u.byteLength);
        var resultArray = new js.html.Uint8Array(result);
		resultArray.set(u);
        return result;
	}

	static function __init__() untyped {
		var ArrayBuffer = __js__('typeof(window) != "undefined" && window.ArrayBuffer') || ArrayBuffer;
		if( ArrayBuffer.prototype.slice == null ) ArrayBuffer.prototype.slice = sliceImpl; // IE10
	}
}
