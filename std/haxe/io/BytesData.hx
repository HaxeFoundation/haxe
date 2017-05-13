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
package haxe.io;

#if neko
	typedef BytesData =	neko.NativeString;
#elseif flash
	typedef BytesData =	flash.utils.ByteArray;
#elseif php
	typedef BytesData = php.BytesData;
#elseif cpp
	typedef BytesData = Array< cpp.UInt8 >;
#elseif java
	typedef BytesData = java.NativeArray<java.StdTypes.Int8>;
#elseif cs
	typedef BytesData = cs.NativeArray<cs.StdTypes.UInt8>;
#elseif python
	typedef BytesData = python.Bytearray;
#elseif js
	typedef BytesData = js.html.ArrayBuffer;
#elseif hl
	class BytesDataImpl {
		public var bytes : hl.Bytes;
		public var length : Int;
		public function new(b,length) {
			this.bytes = b;
			this.length = length;
		}
	}
	@:forward(bytes,length)
	abstract BytesDataAbstract(BytesDataImpl) {
		public inline function new(b, length) {
			this = new BytesDataImpl(b, length);
		}
		@:arrayAccess inline function get(i:Int) return this.bytes[i];
		@:arrayAccess inline function set(i:Int,v:Int) return this.bytes[i] = v;
		@:to inline function toBytes() : hl.Bytes {
			return this == null ? null : this.bytes;
		}
	}
	typedef BytesData = BytesDataAbstract;
#else
	typedef BytesData = Array<Int>;
#end
