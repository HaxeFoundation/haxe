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
package js.html.compat;

#if !nodejs
import js.Lib.nativeThis;

@:ifFeature("js.html.Uint8Array.*")
class Uint8Array {

	static var BYTES_PER_ELEMENT = 1;

	static function _new( ?arg1 : Dynamic, ?offset : Int, ?length : Int ) : Dynamic {
		var arr;
		if( untyped __typeof__(arg1) == 'number' ) {
			arr = new Array();
			for( i in 0...arg1 )
				arr[i] = 0;
			untyped {
				arr.byteLength = arr.length;
				arr.byteOffset = 0;
				arr.buffer = new ArrayBuffer(arr);
			}
		} else if( Std.is(arg1,ArrayBuffer) ) {
			var buffer : ArrayBuffer = arg1;
			if( offset == null ) offset = 0;
			if( length == null ) length = buffer.byteLength - offset;
			if( offset == 0 )
				arr = cast @:privateAccess buffer.a;
			else
				// here we are losing the fact that we should reference the same data,
				// but I don't see another way to have this behaviour while keeping [] access
				arr = cast @:privateAccess buffer.a.slice(offset, offset+length);
			untyped {
				arr.byteLength = arr.length;
				arr.byteOffset = offset;
				arr.buffer = buffer;
			}
		} else if( Std.is(arg1, Array) ) {
			arr = (arg1 : Array<Int>).copy();
			untyped {
				arr.byteLength = arr.length;
				arr.byteOffset = 0;
				arr.buffer = new ArrayBuffer(arr);
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
		if( Std.is(arg.buffer,ArrayBuffer) ) {
			var a : Array<Int> = arg;
			if( arg.byteLength + offset > nativeThis.byteLength )
				throw "set() outside of range";
			for( i in 0...arg.byteLength )
				nativeThis[i + offset] = a[i];
		} else if( Std.is(arg,Array) ) {
			var a : Array<Int> = arg;
			if( a.length + offset > nativeThis.byteLength )
				throw "set() outside of range";
			for( i in 0...a.length )
				nativeThis[i + offset] = a[i];
		} else
			throw "TODO";
	}

	static function _subarray( start : Int, ?end : Int ) {
		var a = _new(nativeThis.slice(start,end));
		a.byteOffset = start;
		return a;
	}

	static function __init__() {
		var Uint8Array = untyped js.Lib.global.Uint8Array || _new;
	}

}
#end