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

package jvm;

import haxe.extern.Rest;
import haxe.Constraints;
import Enum;
import jvm.DynamicObject;
import jvm.Exception;
import jvm.EmptyConstructor;
import jvm.Object;
import jvm.annotation.ClassReflectionInformation;
import jvm.annotation.EnumReflectionInformation;
import jvm.annotation.EnumValueReflectionInformation;
import java.lang.invoke.*;
import java.NativeArray;
import java.Init;
import haxe.ds.Vector;
import haxe.ds.Option;

@:keep
@:native('haxe.jvm.Jvm')
class Jvm {
	extern static public function instanceof<S, T>(obj:S, type:T):Bool;

	extern static public function referenceEquals<T>(v1:T, v2:T):Bool;

	extern static public function invokedynamic<T>(bootstrapMethod:Function, fieldName:String, staticArguments:Array<Dynamic>, rest:Rest<Dynamic>):T;

	static public function stringCompare(v1:String, v2:String):Int {
		if (v1 == null) {
			return v2 == null ? 0 : 1;
		}
		if (v2 == null) {
			return -1;
		}
		return (cast v1 : java.NativeString).compareTo(v2);
	}

	static public function compare<T>(v1:T, v2:T):Int {
		return Reflect.compare(v1, v2);
	}

	// calls

	static public function getArgumentTypes(args:NativeArray<Dynamic>):NativeArray<java.lang.Class<Dynamic>> {
		var argTypes:NativeArray<java.lang.Class<Dynamic>> = new NativeArray(args.length);
		for (i in 0...args.length) {
			var arg = (cast args[i] : java.lang.Object);
			argTypes[i] = arg == null ? (cast java.lang.Object) : arg.getClass();
		}
		return argTypes;
	}

	static public function unifyCallArguments(args:NativeArray<Dynamic>, params:NativeArray<java.lang.Class<Dynamic>>,
			allowPadding:Bool = false):Option<NativeArray<Dynamic>> {
		var callArgs:NativeArray<Dynamic> = {
			if (args.length < params.length) {
				var callArgs = new NativeArray(params.length);
				Vector.blit(cast args, 0, cast callArgs, 0, args.length);
				callArgs;
			} else {
				Vector.fromData(args).copy().toData();
			}
		}
		if (params.length < args.length) {
			return None;
		}
		for (i in 0...params.length) {
			var paramType = params[i];
			if (i >= args.length) {
				if (paramType == cast Bool) {
					callArgs[i] = false;
				} else if (paramType == cast Float) {
					callArgs[i] = 0.0;
				} else if (paramType == cast Int) {
					callArgs[i] = 0;
				} else {
					if (!allowPadding) {
						return None;
					}
					callArgs[i] = null;
				}
				continue;
			}
			var argValue = args[i];
			if (argValue == null) {
				if (paramType.isPrimitive()) {
					if (paramType == cast Bool) {
						callArgs[i] = false;
					} else if (paramType == cast Float) {
						callArgs[i] = 0.0;
					} else if (paramType == cast Int) {
						callArgs[i] = 0;
					} else {
						throw 'Unexpected basic type: $paramType';
					}
				} else {
					callArgs[i] = null;
				}
				continue;
			};
			var argType = (argValue : java.lang.Object).getClass();
			var arg = getWrapperClass(paramType);
			if (arg.isAssignableFrom(argType)) {
				callArgs[i] = args[i];
				continue;
			}
			if (arg == (cast java.lang.Double.DoubleClass) && argType == cast java.lang.Integer.IntegerClass) {
				callArgs[i] = nullIntToNullFloat(args[i]);
			} else {
				return None;
			}
		}
		return Some(callArgs);
	}

	static public function call(mh:java.lang.invoke.MethodHandle, args:NativeArray<Dynamic>) {
		var params = mh.type().parameterArray();
		return switch (unifyCallArguments(args, params, true)) {
			case Some(args): mh.invokeWithArguments(args);
			case None: mh.invokeWithArguments(args);
		}
	}

	// casts

	static public function dynamicToNullFloat<T>(d:T):Null<Float> {
		if (instanceof(d, java.lang.Integer.IntegerClass)) {
			return nullIntToNullFloat(cast d);
		}
		// TODO: need a better strategy to avoid infinite recursion here
		return cast d;
	}

	static public function nullIntToNullFloat(i:Null<Int>):Null<Float> {
		if (i == null) {
			return null;
		}
		return (cast i : java.lang.Number).intValue();
	}

	static public function toByte(d:Dynamic) {
		return d == null ? 0 : (d : java.lang.Byte).byteValue();
	}

	static public function toChar(d:Dynamic) {
		return d == null ? 0 : (d : java.lang.Character).charValue();
	}

	static public function toDouble(d:Dynamic) {
		return d == null ? 0. : (d : java.lang.Number).doubleValue();
	}

	static public function toFloat(d:Dynamic):Single {
		return d == null ? 0. : (d : java.lang.Number).floatValue();
	}

	static public function toInt(d:Dynamic) {
		return d == null ? 0 : (d : java.lang.Number).intValue();
	}

	static public function toLong(d:Dynamic) {
		return d == null ? 0 : (d : java.lang.Long).longValue();
	}

	static public function toShort(d:Dynamic) {
		return d == null ? 0 : (d : java.lang.Short).shortValue();
	}

	static public function toBoolean(d:Dynamic) {
		return d == null ? false : (d : java.lang.Boolean).booleanValue();
	}

	static public function getWrapperClass<S, T>(c:java.lang.Class<S>):java.lang.Class<S> {
		if (!c.isPrimitive()) {
			return c;
		}
		// TODO: other basic types
		return if (c == cast Int) {
			cast java.lang.Integer.IntegerClass;
		} else if (c == cast Float) {
			cast java.lang.Double.DoubleClass;
		} else if (c == cast Bool) {
			cast java.lang.Boolean.BooleanClass;
		} else {
			c;
		}
	}

	// access

	static public function arrayRead(obj:Dynamic, index:Int) {
		if (instanceof(obj, Array)) {
			return (obj : Array<Dynamic>)[index];
		}
		throw 'Cannot array-read on $obj';
	}

	static public function arrayWrite(obj:Dynamic, index:Int, value:Dynamic):Void {
		if (instanceof(obj, Array)) {
			(obj : Array<Dynamic>)[index] = value;
			return;
		}
		throw 'Cannot array-write on $obj';
	}

	static public function bootstrap(caller:MethodHandles.MethodHandles_Lookup, name:String, type:MethodType):CallSite {
		var handle = caller.findStatic(caller.lookupClass(), name, type);
		return new ConstantCallSite(handle);
	}

	static public function readFieldNoObject(obj:Dynamic, name:String):Dynamic {
		var isStatic = instanceof(obj, java.lang.Class);
		var cl = isStatic ? obj : (obj : java.lang.Object).getClass();
		try {
			var field = cl.getField(name);
			field.setAccessible(true);
			return field.get(obj);
		} catch (_:java.lang.NoSuchFieldException) {
			while (cl != null) {
				var methods = cl.getMethods();
				for (m in methods) {
					if (m.getName() == name) {
						var method = java.lang.invoke.MethodHandles.lookup().unreflect(m);
						if (!isStatic || cl == cast java.lang.Class) {
							method = method.bindTo(obj);
						}
						return method;
					}
				}
				if (isStatic) {
					if (cl == cast java.lang.Class) {
						break;
					}
					cl = cast java.lang.Class;
				} else {
					cl = cl.getSuperclass();
				}
			}
			return null;
		}
	}

	static public function readField(obj:Dynamic, name:String):Dynamic {
		if (obj == null || name == null) {
			return null;
		}
		if (instanceof(obj, jvm.Object)) {
			return (cast obj : jvm.Object)._hx_getField(name);
		}
		if (instanceof(obj, java.NativeString)) {
			switch (name) {
				case "length":
					return (obj : String).length;
				case "charAt":
					return (cast jvm.StringExt.charAt : java.lang.invoke.MethodHandle).bindTo(obj);
				case "charCodeAt":
					return (cast jvm.StringExt.charCodeAt : java.lang.invoke.MethodHandle).bindTo(obj);
				case "indexOf":
					return (cast jvm.StringExt.indexOf : java.lang.invoke.MethodHandle).bindTo(obj);
				case "iterator":
					return function() return new haxe.iterators.StringIterator(obj);
				case "keyValueIterator":
					return function() return new haxe.iterators.StringKeyValueIterator(obj);
				case "lastIndexOf":
					return (cast jvm.StringExt.lastIndexOf : java.lang.invoke.MethodHandle).bindTo(obj);
				case "split":
					return (cast jvm.StringExt.split : java.lang.invoke.MethodHandle).bindTo(obj);
				case "substr":
					return (cast jvm.StringExt.substr : java.lang.invoke.MethodHandle).bindTo(obj);
				case "substring":
					return (cast jvm.StringExt.substring : java.lang.invoke.MethodHandle).bindTo(obj);
				case "toLowerCase":
					return (cast jvm.StringExt.toLowerCase : java.lang.invoke.MethodHandle).bindTo(obj);
				case "toUpperCase":
					return (cast jvm.StringExt.toUpperCase : java.lang.invoke.MethodHandle).bindTo(obj);
			}
		}
		return readFieldNoObject(obj, name);
	}

	static public function writeFieldNoObject<T>(obj:Dynamic, name:String, value:T) {
		try {
			var cl = (obj : java.lang.Object).getClass();
			var field = cl.getField(name);
			field.setAccessible(true);
			try {
				field.set(obj, value);
			} catch (_:java.lang.IllegalArgumentException) {
				if (value == null) {
					field.setByte(obj, 0); // rely on widening
				} else if (field.getType() == (cast Int) && instanceof(value, java.lang.Number)) {
					// Can happen with ++ on Dynamic because that defaults to Float
					field.setInt(obj, (cast value : java.lang.Number).intValue());
				}
			}
		} catch (_:java.lang.NoSuchFieldException) {
			return;
		}
	}

	static public function writeField<T>(obj:Dynamic, name:String, value:T) {
		if (obj == null || name == null) {
			return;
		}
		if (instanceof(obj, Object)) {
			return (obj : Object)._hx_setField(name, value);
		}
		writeFieldNoObject(obj, name, value);
	}

	// string

	static public function toString<T:java.lang.Object>(obj:T):String {
		if (obj == null) {
			return "null";
		} else if (instanceof(obj, java.lang.Double.DoubleClass)) {
			var n:java.lang.Number = cast obj;
			if (n.doubleValue() == n.intValue()) {
				return java.lang.Integer.IntegerClass.valueOf(n.intValue()).toString();
			}
			return obj.toString();
		} else {
			return obj.toString();
		}
	}

	static public function stringConcat<A:java.lang.Object, B:java.lang.Object>(a:A, b:B):String {
		return (cast toString(a) : java.NativeString).concat(toString(b));
	}

	// ops

	static public function opAdd<T1:java.lang.Object, T2:java.lang.Object>(a:T1, b:T2):Dynamic {
		if (instanceof(a, java.NativeString) || instanceof(b, java.NativeString)) {
			return stringConcat(a, b);
		}
		if (instanceof(a, java.lang.Double.DoubleClass) || instanceof(b, java.lang.Double.DoubleClass)) {
			return toDouble(a) + toDouble(b);
		}
		if (instanceof(a, java.lang.Long.LongClass) || instanceof(b, java.lang.Long.LongClass)) {
			return toLong(a) + toLong(b);
		}
		if (instanceof(a, java.lang.Integer.IntegerClass) || instanceof(b, java.lang.Integer.IntegerClass)) {
			return toInt(a) + toInt(b);
		}
		throw "Invalid operation";
	}

	static public function opSub<T1:java.lang.Object, T2:java.lang.Object>(a:T1, b:T2):Dynamic {
		if (instanceof(a, java.lang.Double.DoubleClass) || instanceof(b, java.lang.Double.DoubleClass)) {
			return toDouble(a) - toDouble(b);
		}
		if (instanceof(a, java.lang.Long.LongClass) || instanceof(b, java.lang.Long.LongClass)) {
			return toLong(a) - toLong(b);
		}
		if (instanceof(a, java.lang.Integer.IntegerClass) || instanceof(b, java.lang.Integer.IntegerClass)) {
			return toInt(a) - toInt(b);
		}
		throw "Invalid operation";
	}

	static public function opMul<T1:java.lang.Object, T2:java.lang.Object>(a:T1, b:T2):Dynamic {
		if (instanceof(a, java.lang.Double.DoubleClass) || instanceof(b, java.lang.Double.DoubleClass)) {
			return toDouble(a) * toDouble(b);
		}
		if (instanceof(a, java.lang.Long.LongClass) || instanceof(b, java.lang.Long.LongClass)) {
			return toLong(a) * toLong(b);
		}
		if (instanceof(a, java.lang.Integer.IntegerClass) || instanceof(b, java.lang.Integer.IntegerClass)) {
			return toInt(a) * toInt(b);
		}
		throw "Invalid operation";
	}

	static public function opDiv<T1:java.lang.Object, T2:java.lang.Object>(a:T1, b:T2):Dynamic {
		if (instanceof(a, java.lang.Double.DoubleClass) || instanceof(b, java.lang.Double.DoubleClass)) {
			return toDouble(a) / toDouble(b);
		}
		if (instanceof(a, java.lang.Long.LongClass) || instanceof(b, java.lang.Long.LongClass)) {
			return toLong(a) / toLong(b);
		}
		if (instanceof(a, java.lang.Integer.IntegerClass) || instanceof(b, java.lang.Integer.IntegerClass)) {
			return toInt(a) / toInt(b);
		}
		throw "Invalid operation";
	}

	static public function opMod<T1:java.lang.Object, T2:java.lang.Object>(a:T1, b:T2):Dynamic {
		if (instanceof(a, java.lang.Double.DoubleClass) || instanceof(b, java.lang.Double.DoubleClass)) {
			return toDouble(a) % toDouble(b);
		}
		if (instanceof(a, java.lang.Long.LongClass) || instanceof(b, java.lang.Long.LongClass)) {
			return toLong(a) % toLong(b);
		}
		if (instanceof(a, java.lang.Integer.IntegerClass) || instanceof(b, java.lang.Integer.IntegerClass)) {
			return toInt(a) % toInt(b);
		}
		throw "Invalid operation";
	}

	static public function opAnd<T1:java.lang.Object, T2:java.lang.Object>(a:T1, b:T2):Dynamic {
		if (instanceof(a, java.lang.Long.LongClass) || instanceof(b, java.lang.Long.LongClass)) {
			return toLong(a) & toLong(b);
		}
		if (instanceof(a, java.lang.Integer.IntegerClass) || instanceof(b, java.lang.Integer.IntegerClass)) {
			return toInt(a) & toInt(b);
		}
		throw "Invalid operation";
	}

	static public function opOr<T1:java.lang.Object, T2:java.lang.Object>(a:T1, b:T2):Dynamic {
		if (instanceof(a, java.lang.Long.LongClass) || instanceof(b, java.lang.Long.LongClass)) {
			return toLong(a) | toLong(b);
		}
		if (instanceof(a, java.lang.Integer.IntegerClass) || instanceof(b, java.lang.Integer.IntegerClass)) {
			return toInt(a) | toInt(b);
		}
		throw "Invalid operation";
	}

	static public function opXor<T1:java.lang.Object, T2:java.lang.Object>(a:T1, b:T2):Dynamic {
		if (instanceof(a, java.lang.Long.LongClass) || instanceof(b, java.lang.Long.LongClass)) {
			return toLong(a) ^ toLong(b);
		}
		if (instanceof(a, java.lang.Integer.IntegerClass) || instanceof(b, java.lang.Integer.IntegerClass)) {
			return toInt(a) ^ toInt(b);
		}
		throw "Invalid operation";
	}

	static public function opShl<T1:java.lang.Object, T2:java.lang.Object>(a:T1, b:T2):Dynamic {
		if (instanceof(a, java.lang.Long.LongClass) || instanceof(b, java.lang.Long.LongClass)) {
			return toLong(a) << toInt(b);
		}
		if (instanceof(a, java.lang.Integer.IntegerClass) || instanceof(b, java.lang.Integer.IntegerClass)) {
			return toInt(a) << toInt(b);
		}
		throw "Invalid operation";
	}

	static public function opShr<T1:java.lang.Object, T2:java.lang.Object>(a:T1, b:T2):Dynamic {
		if (instanceof(a, java.lang.Long.LongClass) || instanceof(b, java.lang.Long.LongClass)) {
			return toLong(a) >> toInt(b);
		}
		if (instanceof(a, java.lang.Integer.IntegerClass) || instanceof(b, java.lang.Integer.IntegerClass)) {
			return toInt(a) >> toInt(b);
		}
		throw "Invalid operation";
	}

	static public function opUshr<T1:java.lang.Object, T2:java.lang.Object>(a:T1, b:T2):Dynamic {
		if (instanceof(a, java.lang.Long.LongClass) || instanceof(b, java.lang.Long.LongClass)) {
			return toLong(a) >>> toInt(b);
		}
		if (instanceof(a, java.lang.Integer.IntegerClass) || instanceof(b, java.lang.Integer.IntegerClass)) {
			return toInt(a) >>> toInt(b);
		}
		throw "Invalid operation";
	}

	static public function opIncrement<T1:java.lang.Object>(a:T1):Dynamic {
		if (instanceof(a, java.lang.Double.DoubleClass)) {
			return toDouble(a) + 1.;
		}
		if (instanceof(a, java.lang.Long.LongClass)) {
			return toLong(a) + 1.;
		}
		if (instanceof(a, java.lang.Integer.IntegerClass)) {
			return toInt(a) + 1;
		}
		throw "Invalid operation";
	}

	static public function opDecrement<T1:java.lang.Object>(a:T1):Dynamic {
		if (instanceof(a, java.lang.Double.DoubleClass)) {
			return toDouble(a) - 1.;
		}
		if (instanceof(a, java.lang.Long.LongClass)) {
			return toLong(a) - 1.;
		}
		if (instanceof(a, java.lang.Integer.IntegerClass)) {
			return toInt(a) - 1;
		}
		throw "Invalid operation";
	}

	static public function opNeg<T1:java.lang.Object>(a:T1):Dynamic {
		if (instanceof(a, java.lang.Double.DoubleClass)) {
			return -toDouble(a);
		}
		if (instanceof(a, java.lang.Long.LongClass)) {
			return -toLong(a);
		}
		if (instanceof(a, java.lang.Integer.IntegerClass)) {
			return -toInt(a);
		}
		throw "Invalid operation";
	}

	static public function opNegBits<T1:java.lang.Object>(a:T1):Dynamic {
		if (instanceof(a, java.lang.Long.LongClass)) {
			return ~toLong(a);
		}
		if (instanceof(a, java.lang.Integer.IntegerClass)) {
			return ~toInt(a);
		}
		throw "Invalid operation";
	}
}
