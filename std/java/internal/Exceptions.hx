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
package java.internal;
import java.lang.Throwable;
import java.lang.RuntimeException;
import java.lang.Exception;

@:native("haxe.lang.Exceptions")
class Exceptions {
	private static var exception = new java.lang.ThreadLocal<java.lang.Throwable>();

	@:keep private static function setException(exc:Throwable)
	{
		exception.set(exc);
	}

	public static function currentException()
	{
		return exception.get();
	}
}

@:classCode("public static final long serialVersionUID = 5956463319488556322L;")
@:nativeGen @:keep @:native("haxe.lang.HaxeException") private class HaxeException extends RuntimeException
{
	private var obj:Dynamic;

	public function new(obj:Dynamic, msg:String, cause:Throwable)
	{
		super(msg, cause);

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

#if !debug
	@:overload override public function fillInStackTrace():Throwable
	{
		return this;
	}
#end

	@:overload override public function toString():String
	{
		return "Haxe Exception: " + obj;
	}
	
	@:overload override public function getMessage():String
	{
		return switch (super.getMessage())
		{
			case null: Std.string(obj);
			case message: message;
		}
	}

	public static function wrap(obj:Dynamic):RuntimeException
	{
		var ret:RuntimeException = null;
 		if (Std.is(obj, RuntimeException))
			ret = obj;
		else if (Std.is(obj, String))
			ret = new HaxeException(obj, obj, null);
 		else if (Std.is(obj, Throwable))
			ret = new HaxeException(obj, Std.string(obj), obj);
		else
			ret = new HaxeException(obj, Std.string(obj), null);
		return ret;
	}
}
