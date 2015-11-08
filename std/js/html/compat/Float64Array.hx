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

#if !nodejs
@:keep
class Float64Array {

	static var BYTES_PER_ELEMENT = 8;

	static function _new( ?arg1 : Dynamic, ?offset : Int, ?length : Int ) : Dynamic {
		var arr : Array<Float>;
		if( untyped __typeof__(arg1) == 'number' ) {
			arr = new Array();
			for( i in 0...arg1 )
				arr[i] = 0;
			untyped {
				arr.byteLength = arr.length << 3;
				arr.byteOffset = 0;
				arr.buffer = new ArrayBuffer([for( i in 0...arr.length << 3 ) 0]); // no sync
			}
		} else if( Std.is(arg1,ArrayBuffer) ) {
			var buffer : ArrayBuffer = arg1;
			if( offset == null ) offset = 0;
			if( length == null ) length = (buffer.byteLength - offset) >> 3;
			arr = [];
			// decode buffer
			for( i in 0...length ) {
				var val1 = untyped buffer.a[offset++] | (buffer.a[offset++] << 8) | (buffer.a[offset++] << 16) | (buffer.a[offset++] << 24);
				var val2 = untyped buffer.a[offset++] | (buffer.a[offset++] << 8) | (buffer.a[offset++] << 16) | (buffer.a[offset++] << 24);
				arr.push(haxe.io.FPHelper.i64ToDouble(val1,val2));
			}
			untyped {
				arr.byteLength = arr.length<<3;
				arr.byteOffset = offset;
				arr.buffer = buffer;
			}
		} else if( Std.is(arg1, Array) ) {
			arr = (arg1 : Array<Float>).copy();
			// loss of memory sync between buffer and array
			var buffer = [];
			for( f in arr ) {
				var v = haxe.io.FPHelper.doubleToI64(f);
				var i = v.low;
				buffer.push(i&0xFF);
				buffer.push((i>>8)&0xFF);
				buffer.push((i>>16)&0xFF);
				buffer.push(i>>>24);
				var i = v.high;
				buffer.push(i&0xFF);
				buffer.push((i>>8)&0xFF);
				buffer.push((i>>16)&0xFF);
				buffer.push(i>>>24);
			}
			untyped {
				arr.byteLength = arr.length << 3;
				arr.byteOffset = 0;
				arr.buffer = new ArrayBuffer(buffer);
			}
		} else
			throw "TODO "+arg1;
		untyped {
			arr.subarray = _subarray;
			arr.set = _set;
		}
		return arr;
	}

	static function _set( ?arg : Dynamic, ?offset : Int ) {
		var t : Dynamic = untyped __js__("this");
		if( Std.is(arg.buffer,ArrayBuffer) ) {
			var a : Array<Int> = arg;
			if( arg.byteLength + offset > t.byteLength )
				throw "set() outside of range";
			for( i in 0...arg.byteLength )
				t[i + offset] = a[i];
		} else if( Std.is(arg,Array) ) {
			var a : Array<Int> = arg;
			if( a.length + offset > t.byteLength )
				throw "set() outside of range";
			for( i in 0...a.length )
				t[i + offset] = a[i];
		} else
			throw "TODO";
	}

	static function _subarray( start : Int, ?end : Int ) {
		var t : Dynamic = untyped __js__("this");
		var a = _new(t.slice(start,end));
		a.byteOffset = start * 8;
		return a;
	}

	static function __init__() {
		var Float64Array = untyped js.Lib.global.Float64Array || (js.Lib.global.Float32Array ? 'notsupported' : null) || _new;
	}

}
#end
