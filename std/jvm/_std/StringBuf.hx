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
@:coreApi
class StringBuf {
	private var b:java.lang.StringBuilder;

	public var length(get, never):Int;

	public function new():Void {
		b = new java.lang.StringBuilder();
	}

	inline function get_length():Int {
		return b.length();
	}

	public function add<T>(x:T):Void {
		if (jvm.Jvm.instanceof(x, java.lang.Double.DoubleClass)) {
			b.append(jvm.Jvm.toString(cast x));
		} else {
			b.append(x);
		}
	}

	@:overload
	@:native("add")
	@:ifFeature("StringBuf.add")
	function addOpt(v:Bool):Void {
		b.append(v);
	}

	@:overload
	@:native("add")
	@:ifFeature("StringBuf.add")
	function addOpt(v:java.types.Char16):Void {
		b.append(v);
	}

	@:overload
	@:native("add")
	@:ifFeature("StringBuf.add")
	function addOpt(v:Float):Void {
		b.append(v);
	}

	@:overload
	@:native("add")
	@:ifFeature("StringBuf.add")
	function addOpt(v:Single):Void {
		b.append(v);
	}

	@:overload
	@:native("add")
	@:ifFeature("StringBuf.add")
	function addOpt(v:Int):Void {
		b.append(v);
	}

	@:overload
	@:native("add")
	@:ifFeature("StringBuf.add")
	function addOpt(v:haxe.Int64):Void {
		b.append(v);
	}

	@:overload
	@:native("add")
	@:ifFeature("StringBuf.add")
	function addOpt(v:String):Void {
		b.append(v);
	}

	public function addSub(s:String, pos:Int, ?len:Int):Void {
		var l:Int = (len == null) ? s.length - pos : len;
		b.append(s, pos, pos + l);
	}

	public function addChar(c:Int):Void {
		b.appendCodePoint(c);
	}

	public function toString():String {
		return b.toString();
	}
}
