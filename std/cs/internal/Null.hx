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

@:classCode('//This function is here to be used with Reflection, when the haxe.lang.Null type is known
		public static haxe.lang.Null<T> _ofDynamic(object obj) {
			if (obj == null) {
				return new haxe.lang.Null<T>(default(T), false);
			} else if (typeof(T).Equals(typeof(double))) {
				return new haxe.lang.Null<T>((T) (object) haxe.lang.Runtime.toDouble(obj), true);
			} else if (typeof(T).Equals(typeof(int))) {
				return new haxe.lang.Null<T>((T) (object) haxe.lang.Runtime.toInt(obj), true);
			} else {
				return new haxe.lang.Null<T>((T) obj, true);
			}
		}

		public static implicit operator haxe.lang.Null<T>(T val) {
			return new haxe.lang.Null<T>(val, true);
		}

		public static implicit operator Null<T>(__NoValue__ noValue) {
			return new haxe.lang.Null<T>(default(T), false);
		}

		public sealed class __NoValue__ {
			private __NoValue__() {}
		}

')
#if core_api_serialize
@:meta(System.Serializable)
#end
@:keep @:struct @:nativeGen @:native("haxe.lang.Null") private class Nullable<T>
{

	@:readOnly public var value(default,never):T;
	@:readOnly public var hasValue(default,never):Bool;

	public function new(v:T, hasValue:Bool)
	{
		if (hasValue && cs.system.Object.ReferenceEquals(v, null))
		{
			hasValue = false;
		}
		untyped this.value = v;
		untyped this.hasValue = hasValue;
	}

	@:functionCode('if (obj == null) {
				return new haxe.lang.Null<D>(default(D), false);
			} else if (typeof(D).Equals(typeof(double))) {
				return new haxe.lang.Null<D>((D) (object) haxe.lang.Runtime.toDouble(obj), true);
			} else if (typeof(D).Equals(typeof(int))) {
				return new haxe.lang.Null<D>((D) (object) haxe.lang.Runtime.toInt(obj), true);
			} else {
				return new haxe.lang.Null<D>((D) obj, true);
			}')
	public static function ofDynamic<D>(obj:Dynamic):Nullable<D>
	{
		return null;
	}

	public function toDynamic():Dynamic
	{
		if (this.hasValue)
			return value;
		return null;
	}
}
