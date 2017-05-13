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
package cs.internal;
import cs.system.Exception;

@:nativeGen @:keep @:native("haxe.lang.Exceptions") class Exceptions {

	@:allow(haxe.CallStack)
	@:meta(System.ThreadStaticAttribute)
	static var exception:cs.system.Exception;
}

//should NOT be usable inside Haxe code
@:classCode('override public string Message { get { return this.toString(); } }\n\n')
@:nativeGen @:keep @:native("haxe.lang.HaxeException") private class HaxeException extends Exception
{
	private var obj:Dynamic;

	public function new(obj:Dynamic)
	{
		super();

		if (Std.is(obj, HaxeException))
		{
			var _obj:HaxeException = cast obj;
			obj = _obj.getObject();
		}
		this.obj = obj;
	}

	public function getObject():Dynamic
	{
		return obj;
	}

	public function toString():String
	{
		return Std.string(obj);
	}

	public static function wrap(obj:Dynamic):Exception
	{
		if (Std.is(obj, Exception)) return obj;

		return new HaxeException(obj);
	}
}
