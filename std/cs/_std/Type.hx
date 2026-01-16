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

enum ValueType {
	TNull;
	TInt;
	TInt64;
	TFloat;
	TBool;
	TObject;
	TFunction;
	TClass(c:Class<Dynamic>);
	TEnum(e:Enum<Dynamic>);
	TUnknown;
}

@:coreApi
class Type {
	public static function getClass<T>(o:T):Null<Class<T>> {
		if (o == null)
			return null;
		// TODO: proper implementation using reflection
		return null;
	}

	public static function getEnum(o:EnumValue):Null<Enum<Dynamic>> {
		if (o == null)
			return null;
		// TODO: proper implementation
		return null;
	}

	public static function getSuperClass(c:Class<Dynamic>):Null<Class<Dynamic>> {
		// TODO: proper implementation
		return null;
	}

	public static function getClassName(c:Class<Dynamic>):String {
		if (c == null)
			return null;
		// TODO: proper implementation
		return "Unknown";
	}

	public static function getEnumName(e:Enum<Dynamic>):String {
		if (e == null)
			return null;
		// TODO: proper implementation
		return "Unknown";
	}

	public static function resolveClass(name:String):Null<Class<Dynamic>> {
		// TODO: proper implementation
		return null;
	}

	public static function resolveEnum(name:String):Null<Enum<Dynamic>> {
		// TODO: proper implementation
		return null;
	}

	public static function createInstance<T>(cl:Class<T>, args:Array<Dynamic>):T {
		// TODO: proper implementation
		throw new haxe.exceptions.NotImplementedException();
	}

	public static function createEmptyInstance<T>(cl:Class<T>):T {
		// TODO: proper implementation
		throw new haxe.exceptions.NotImplementedException();
	}

	public static function createEnum<T>(e:Enum<T>, constr:String, ?params:Array<Dynamic>):T {
		// TODO: proper implementation
		throw new haxe.exceptions.NotImplementedException();
	}

	public static function createEnumIndex<T>(e:Enum<T>, index:Int, ?params:Array<Dynamic>):T {
		// TODO: proper implementation
		throw new haxe.exceptions.NotImplementedException();
	}

	public static function getInstanceFields(c:Class<Dynamic>):Array<String> {
		// TODO: proper implementation
		return [];
	}

	public static function getClassFields(c:Class<Dynamic>):Array<String> {
		// TODO: proper implementation
		return [];
	}

	public static function getEnumConstructs(e:Enum<Dynamic>):Array<String> {
		// TODO: proper implementation
		return [];
	}

	public static function typeof(v:Dynamic):ValueType {
		if (v == null)
			return TNull;
		// TODO: proper implementation
		return TUnknown;
	}

	public static function enumEq<T:EnumValue>(a:T, b:T):Bool {
		if (a == null)
			return b == null;
		if (b == null)
			return false;
		// TODO: proper implementation
		return a == b;
	}

	public static function enumConstructor(e:EnumValue):String {
		// TODO: proper implementation
		return "Unknown";
	}

	public static function enumParameters(e:EnumValue):Array<Dynamic> {
		// TODO: proper implementation
		return [];
	}

	public static function enumIndex(e:EnumValue):Int {
		// TODO: proper implementation
		return 0;
	}

	public static function allEnums<T>(e:Enum<T>):Array<T> {
		// TODO: proper implementation
		return [];
	}
}
