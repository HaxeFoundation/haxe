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
class Reflect {
	public static function hasField(o:Dynamic, field:String):Bool {
		if (o == null)
			return false;
		// TODO: proper implementation
		return false;
	}

	public static function field(o:Dynamic, field:String):Dynamic {
		if (o == null)
			return null;
		// TODO: proper implementation
		return null;
	}

	public static function setField(o:Dynamic, field:String, value:Dynamic):Void {
		// TODO: proper implementation
	}

	public static function getProperty(o:Dynamic, field:String):Dynamic {
		// TODO: proper implementation
		return field(o, field);
	}

	public static function setProperty(o:Dynamic, field:String, value:Dynamic):Void {
		// TODO: proper implementation
		setField(o, field, value);
	}

	public static function callMethod(o:Dynamic, func:haxe.Constraints.Function, args:Array<Dynamic>):Dynamic {
		// TODO: proper implementation
		throw new haxe.exceptions.NotImplementedException();
	}

	public static function fields(o:Dynamic):Array<String> {
		// TODO: proper implementation
		return [];
	}

	public static function isFunction(f:Dynamic):Bool {
		// TODO: proper implementation
		return false;
	}

	public static function compare<T>(a:T, b:T):Int {
		if (a == b)
			return 0;
		if (a == null)
			return -1;
		if (b == null)
			return 1;
		// TODO: proper implementation
		return 0;
	}

	public static function compareMethods(f1:Dynamic, f2:Dynamic):Bool {
		// TODO: proper implementation
		return f1 == f2;
	}

	public static function isObject(v:Dynamic):Bool {
		if (v == null)
			return false;
		// TODO: proper implementation
		return true;
	}

	public static function isEnumValue(v:Dynamic):Bool {
		// TODO: proper implementation
		return false;
	}

	public static function deleteField(o:Dynamic, field:String):Bool {
		// TODO: proper implementation
		return false;
	}

	public static function copy<T>(o:Null<T>):Null<T> {
		if (o == null)
			return null;
		// TODO: proper implementation
		return o;
	}

	public static function makeVarArgs(f:Array<Dynamic>->Dynamic):Dynamic {
		// TODO: proper implementation
		throw new haxe.exceptions.NotImplementedException();
	}
}
