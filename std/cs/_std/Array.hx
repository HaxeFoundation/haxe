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

import haxe.iterators.ArrayKeyValueIterator;

/**
 * Internal enum for tracking array storage type.
 * Uses enum abstract backed by Int for efficient switch-based dispatch.
 */
private enum abstract ArrayType(Int) {
	var Unknown = 0;
	var TInt = 1;
	var TFloat = 2;
	var TBool = 3;
	var TObject = 4;
}

/**
 * Haxe Array implementation for C# target.
 *
 * This implementation uses multiple backing arrays (int[], double[], bool[], object[])
 * with only ONE active at a time. The compiler (gencs.ml) generates typed method calls
 * based on the Haxe AST type.
 *
 * Storage type tracking follows Hugh Sando's rules:
 *   Unknown -> Int/Float/Bool/Object (first element determines type)
 *   Int -> Float (when float added via Dyn method)
 *   Int -> Object (when object/string/null added)
 *   Float -> Object (when object/string/null added)
 *   Bool -> Object (when anything else added)
 *   Object -> Object (stays Object forever)
 *
 * The __objectArray field is the object backing array (object[]).
 * gencs.ml generates code that accesses __objectArray directly for array operations.
 */
@:coreApi final class Array<T> implements ArrayAccess<T> {
	public var length(default, null):Int;

	// Type tracking
	private var __arrayType:ArrayType;
	private var __locked:Bool;  // Once true, storage type cannot change

	// Typed backing arrays - only ONE is active at a time
	// __objectArray is used when __arrayType == TObject or Unknown
	@:unreflective private var __intArray:cs.NativeArray<Int>;
	@:unreflective private var __floatArray:cs.NativeArray<Float>;
	@:unreflective private var __boolArray:cs.NativeArray<Bool>;
	@:unreflective private var __objectArray:cs.NativeArray<Dynamic>;

	@:unreflective static var __hx_toString_depth = 0;
	@:unreflective static inline final __hx_defaultCapacity = 4;

	// =========================================================================
	// Constructors and Factory Methods
	// =========================================================================

	public function new():Void {
		this.length = 0;
		this.__arrayType = Unknown;
		this.__locked = false;
		// Start with empty object array for compatibility
		this.__objectArray = new cs.NativeArray<Dynamic>(0);
	}

	private static inline function ofNative<X>(native:cs.NativeArray<Dynamic>):Array<X> {
		var a = new Array<X>();
		a.length = native.length;
		// Use the native array directly as object storage
		a.__objectArray = native;
		a.__arrayType = TObject;
		return a;
	}

	private static inline function alloc<Y>(size:Int):Array<Y> {
		var a = new Array<Y>();
		a.length = size;
		a.__objectArray = new cs.NativeArray<Dynamic>(size);
		a.__arrayType = TObject;
		return a;
	}

	// =========================================================================
	// Haxe API Methods (use __objectArray for backwards compatibility)
	// =========================================================================

	public function concat(a:Array<T>):Array<T> {
		var len = length + a.length;
		// Determine target storage type - prefer to keep typed if both arrays match
		if (__arrayType == TInt && (a.__arrayType == TInt || a.length == 0)) {
			var retarr = new cs.NativeArray<Int>(len);
			cs.NativeArray.arraycopy(__intArray, 0, retarr, 0, length);
			if (a.length > 0)
				cs.NativeArray.arraycopy(a.__intArray, 0, retarr, length, a.length);
			return cast __ofIntLiteral(retarr);
		} else if (__arrayType == TFloat && (a.__arrayType == TFloat || a.length == 0)) {
			var retarr = new cs.NativeArray<Float>(len);
			cs.NativeArray.arraycopy(__floatArray, 0, retarr, 0, length);
			if (a.length > 0)
				cs.NativeArray.arraycopy(a.__floatArray, 0, retarr, length, a.length);
			return cast __ofFloatLiteral(retarr);
		} else if (__arrayType == TBool && (a.__arrayType == TBool || a.length == 0)) {
			var retarr = new cs.NativeArray<Bool>(len);
			cs.NativeArray.arraycopy(__boolArray, 0, retarr, 0, length);
			if (a.length > 0)
				cs.NativeArray.arraycopy(a.__boolArray, 0, retarr, length, a.length);
			return cast __ofBoolLiteral(retarr);
		} else {
			// Fall back to object array - copy from whatever backing each has
			var retarr = new cs.NativeArray<Dynamic>(len);
			for (i in 0...length)
				retarr[i] = __getDyn(i);
			for (i in 0...a.length)
				retarr[length + i] = a.__getDyn(i);
			return ofNative(retarr);
		}
	}

	public function join(sep:String):String {
		var buf = new StringBuf();
		var first = true;
		for (i in 0...length) {
			if (first)
				first = false;
			else
				buf.add(sep);
			buf.add(__getDyn(i));
		}
		return buf.toString();
	}

	public function pop():Null<T> {
		if (length == 0)
			return null;
		--length;
		switch (__arrayType) {
			case TInt:
				var v:Dynamic = __intArray[length];
				__intArray[length] = 0;
				return v;
			case TFloat:
				var v:Dynamic = __floatArray[length];
				__floatArray[length] = 0.0;
				return v;
			case TBool:
				var v:Dynamic = __boolArray[length];
				__boolArray[length] = false;
				return v;
			case TObject, Unknown:
				var v = __objectArray[length];
				__objectArray[length] = null;
				return v;
		}
	}

	public function push(x:T):Int {
		// Push to the active backing array based on __arrayType
		switch (__arrayType) {
			case TInt:
				__ensureIntCapacity(length);
				__intArray[length] = cast x;
				return ++length;
			case TFloat:
				__ensureFloatCapacity(length);
				__floatArray[length] = cast x;
				return ++length;
			case TBool:
				__ensureBoolCapacity(length);
				__boolArray[length] = cast x;
				return ++length;
			case TObject:
				__ensureObjectCapacity(length);
				__objectArray[length] = x;
				return ++length;
			case Unknown:
				// First element - use object storage
				__initObjectArray(length);
				__arrayType = TObject;
				__objectArray[length] = x;
				return ++length;
		}
	}

	public function reverse():Void {
		var i = 0;
		var l = length;
		var half = l >> 1;
		l -= 1;
		switch (__arrayType) {
			case TInt:
				while (i < half) {
					var tmp = __intArray[i];
					__intArray[i] = __intArray[l - i];
					__intArray[l - i] = tmp;
					i += 1;
				}
			case TFloat:
				while (i < half) {
					var tmp = __floatArray[i];
					__floatArray[i] = __floatArray[l - i];
					__floatArray[l - i] = tmp;
					i += 1;
				}
			case TBool:
				while (i < half) {
					var tmp = __boolArray[i];
					__boolArray[i] = __boolArray[l - i];
					__boolArray[l - i] = tmp;
					i += 1;
				}
			case TObject, Unknown:
				while (i < half) {
					var tmp = __objectArray[i];
					__objectArray[i] = __objectArray[l - i];
					__objectArray[l - i] = tmp;
					i += 1;
				}
		}
	}

	public function shift():Null<T> {
		if (length == 0)
			return null;

		var x:Dynamic;
		switch (__arrayType) {
			case TInt:
				x = __intArray[0];
				cs.NativeArray.arraycopy(__intArray, 1, __intArray, 0, length - 1);
				__intArray[length - 1] = 0;
			case TFloat:
				x = __floatArray[0];
				cs.NativeArray.arraycopy(__floatArray, 1, __floatArray, 0, length - 1);
				__floatArray[length - 1] = 0.0;
			case TBool:
				x = __boolArray[0];
				cs.NativeArray.arraycopy(__boolArray, 1, __boolArray, 0, length - 1);
				__boolArray[length - 1] = false;
			case TObject, Unknown:
				x = __objectArray[0];
				cs.NativeArray.arraycopy(__objectArray, 1, __objectArray, 0, length - 1);
				__objectArray[length - 1] = null;
		}
		length -= 1;
		return x;
	}

	public function slice(pos:Int, ?end:Int):Array<T> {
		if (pos < 0) {
			pos = length + pos;
			if (pos < 0)
				pos = 0;
		}
		if (end == null)
			end = length;
		else if (end < 0)
			end = length + end;
		if (end > length)
			end = length;
		var len = end - pos;
		if (len < 0)
			return new Array();

		switch (__arrayType) {
			case TInt:
				var newarr = new cs.NativeArray<Int>(len);
				cs.NativeArray.arraycopy(__intArray, pos, newarr, 0, len);
				return cast __ofIntLiteral(newarr);
			case TFloat:
				var newarr = new cs.NativeArray<Float>(len);
				cs.NativeArray.arraycopy(__floatArray, pos, newarr, 0, len);
				return cast __ofFloatLiteral(newarr);
			case TBool:
				var newarr = new cs.NativeArray<Bool>(len);
				cs.NativeArray.arraycopy(__boolArray, pos, newarr, 0, len);
				return cast __ofBoolLiteral(newarr);
			default:
				var newarr = new cs.NativeArray<Dynamic>(len);
				cs.NativeArray.arraycopy(__objectArray, pos, newarr, 0, len);
				return ofNative(newarr);
		}
	}

	public function sort(f:T->T->Int):Void {
		if (length == 0)
			return;
		// Upgrade to object for sorting to simplify implementation
		if (__arrayType != TObject && __arrayType != Unknown) {
			__upgradeToObject();
		}
		quicksort(0, length - 1, f);
	}

	private function quicksort(lo:Int, hi:Int, f:T->T->Int):Void {
		var i = lo, j = hi;
		var p:T = cast __objectArray[(i + j) >> 1];
		while (i <= j) {
			while (i < hi && f(cast __objectArray[i], p) < 0)
				i++;
			while (j > lo && f(cast __objectArray[j], p) > 0)
				j--;
			if (i <= j) {
				var t = __objectArray[i];
				__objectArray[i++] = __objectArray[j];
				__objectArray[j--] = t;
			}
		}
		if (lo < j)
			quicksort(lo, j, f);
		if (i < hi)
			quicksort(i, hi, f);
	}

	public function splice(pos:Int, len:Int):Array<T> {
		if (len < 0)
			return new Array();
		if (pos < 0) {
			pos = length + pos;
			if (pos < 0)
				pos = 0;
		}
		if (pos > length) {
			pos = 0;
			len = 0;
		} else if (pos + len > length) {
			len = length - pos;
			if (len < 0)
				len = 0;
		}

		var end = pos + len;
		switch (__arrayType) {
			case TInt:
				var ret = new cs.NativeArray<Int>(len);
				cs.NativeArray.arraycopy(__intArray, pos, ret, 0, len);
				cs.NativeArray.arraycopy(__intArray, end, __intArray, pos, length - end);
				length -= len;
				var clearLen = len;
				while (--clearLen >= 0)
					__intArray[length + clearLen] = 0;
				return cast __ofIntLiteral(ret);
			case TFloat:
				var ret = new cs.NativeArray<Float>(len);
				cs.NativeArray.arraycopy(__floatArray, pos, ret, 0, len);
				cs.NativeArray.arraycopy(__floatArray, end, __floatArray, pos, length - end);
				length -= len;
				var clearLen = len;
				while (--clearLen >= 0)
					__floatArray[length + clearLen] = 0.0;
				return cast __ofFloatLiteral(ret);
			case TBool:
				var ret = new cs.NativeArray<Bool>(len);
				cs.NativeArray.arraycopy(__boolArray, pos, ret, 0, len);
				cs.NativeArray.arraycopy(__boolArray, end, __boolArray, pos, length - end);
				length -= len;
				var clearLen = len;
				while (--clearLen >= 0)
					__boolArray[length + clearLen] = false;
				return cast __ofBoolLiteral(ret);
			default:
				var ret = new cs.NativeArray<Dynamic>(len);
				cs.NativeArray.arraycopy(__objectArray, pos, ret, 0, len);
				cs.NativeArray.arraycopy(__objectArray, end, __objectArray, pos, length - end);
				length -= len;
				var clearLen = len;
				while (--clearLen >= 0)
					__objectArray[length + clearLen] = null;
				return ofNative(ret);
		}
	}

	public function toString():String {
		if (__hx_toString_depth >= 5)
			return "...";
		++__hx_toString_depth;
		try {
			var s = __hx_toString();
			--__hx_toString_depth;
			return s;
		} catch (e:Dynamic) {
			--__hx_toString_depth;
			throw(e);
		}
	}

	function __hx_toString():String {
		var ret = new StringBuf();
		ret.add("[");
		var first = true;
		for (i in 0...length) {
			if (first)
				first = false;
			else
				ret.add(",");
			ret.add(__getDyn(i));
		}
		ret.add("]");
		return ret.toString();
	}

	public function unshift(x:T):Void {
		switch (__arrayType) {
			case TInt:
				if (__intArray == null || length >= __intArray.length) {
					var newLen = (length << 1) + 1;
					if (newLen < __hx_defaultCapacity)
						newLen = __hx_defaultCapacity;
					var newarr = new cs.NativeArray<Int>(newLen);
					if (__intArray != null)
						cs.NativeArray.arraycopy(__intArray, 0, newarr, 1, length);
					__intArray = newarr;
				} else {
					cs.NativeArray.arraycopy(__intArray, 0, __intArray, 1, length);
				}
				__intArray[0] = cast x;
			case TFloat:
				if (__floatArray == null || length >= __floatArray.length) {
					var newLen = (length << 1) + 1;
					if (newLen < __hx_defaultCapacity)
						newLen = __hx_defaultCapacity;
					var newarr = new cs.NativeArray<Float>(newLen);
					if (__floatArray != null)
						cs.NativeArray.arraycopy(__floatArray, 0, newarr, 1, length);
					__floatArray = newarr;
				} else {
					cs.NativeArray.arraycopy(__floatArray, 0, __floatArray, 1, length);
				}
				__floatArray[0] = cast x;
			case TBool:
				if (__boolArray == null || length >= __boolArray.length) {
					var newLen = (length << 1) + 1;
					if (newLen < __hx_defaultCapacity)
						newLen = __hx_defaultCapacity;
					var newarr = new cs.NativeArray<Bool>(newLen);
					if (__boolArray != null)
						cs.NativeArray.arraycopy(__boolArray, 0, newarr, 1, length);
					__boolArray = newarr;
				} else {
					cs.NativeArray.arraycopy(__boolArray, 0, __boolArray, 1, length);
				}
				__boolArray[0] = cast x;
			default:
				if (__arrayType == Unknown) {
					__initObjectArray(length);
					__arrayType = TObject;
				}
				if (length >= __objectArray.length) {
					var newLen = (length << 1) + 1;
					var newarr = new cs.NativeArray<Dynamic>(newLen);
					cs.NativeArray.arraycopy(__objectArray, 0, newarr, 1, length);
					__objectArray = newarr;
				} else {
					cs.NativeArray.arraycopy(__objectArray, 0, __objectArray, 1, length);
				}
				__objectArray[0] = x;
		}
		++length;
	}

	public function insert(pos:Int, x:T):Void {
		if (pos < 0) {
			pos = length + pos;
			if (pos < 0)
				pos = 0;
		}
		if (pos >= length) {
			push(x);
			return;
		} else if (pos == 0) {
			unshift(x);
			return;
		}

		switch (__arrayType) {
			case TInt:
				if (__intArray == null || length >= __intArray.length) {
					var newLen = (length << 1) + 1;
					if (newLen < __hx_defaultCapacity)
						newLen = __hx_defaultCapacity;
					var newarr = new cs.NativeArray<Int>(newLen);
					cs.NativeArray.arraycopy(__intArray, 0, newarr, 0, pos);
					newarr[pos] = cast x;
					cs.NativeArray.arraycopy(__intArray, pos, newarr, pos + 1, length - pos);
					__intArray = newarr;
				} else {
					cs.NativeArray.arraycopy(__intArray, pos, __intArray, pos + 1, length - pos);
					__intArray[pos] = cast x;
				}
			case TFloat:
				if (__floatArray == null || length >= __floatArray.length) {
					var newLen = (length << 1) + 1;
					if (newLen < __hx_defaultCapacity)
						newLen = __hx_defaultCapacity;
					var newarr = new cs.NativeArray<Float>(newLen);
					cs.NativeArray.arraycopy(__floatArray, 0, newarr, 0, pos);
					newarr[pos] = cast x;
					cs.NativeArray.arraycopy(__floatArray, pos, newarr, pos + 1, length - pos);
					__floatArray = newarr;
				} else {
					cs.NativeArray.arraycopy(__floatArray, pos, __floatArray, pos + 1, length - pos);
					__floatArray[pos] = cast x;
				}
			case TBool:
				if (__boolArray == null || length >= __boolArray.length) {
					var newLen = (length << 1) + 1;
					if (newLen < __hx_defaultCapacity)
						newLen = __hx_defaultCapacity;
					var newarr = new cs.NativeArray<Bool>(newLen);
					cs.NativeArray.arraycopy(__boolArray, 0, newarr, 0, pos);
					newarr[pos] = cast x;
					cs.NativeArray.arraycopy(__boolArray, pos, newarr, pos + 1, length - pos);
					__boolArray = newarr;
				} else {
					cs.NativeArray.arraycopy(__boolArray, pos, __boolArray, pos + 1, length - pos);
					__boolArray[pos] = cast x;
				}
			default:
				if (__arrayType == Unknown) {
					__initObjectArray(length);
					__arrayType = TObject;
				}
				if (length >= __objectArray.length) {
					var newLen = (length << 1) + 1;
					var newarr = new cs.NativeArray<Dynamic>(newLen);
					cs.NativeArray.arraycopy(__objectArray, 0, newarr, 0, pos);
					newarr[pos] = x;
					cs.NativeArray.arraycopy(__objectArray, pos, newarr, pos + 1, length - pos);
					__objectArray = newarr;
				} else {
					cs.NativeArray.arraycopy(__objectArray, pos, __objectArray, pos + 1, length - pos);
					__objectArray[pos] = x;
				}
		}
		++length;
	}

	public function remove(x:T):Bool {
		var idx = indexOf(x);
		if (idx >= 0) {
			// Remove element at idx by shifting elements
			switch (__arrayType) {
				case TInt:
					cs.NativeArray.arraycopy(__intArray, idx + 1, __intArray, idx, length - idx - 1);
					__intArray[--length] = 0;
				case TFloat:
					cs.NativeArray.arraycopy(__floatArray, idx + 1, __floatArray, idx, length - idx - 1);
					__floatArray[--length] = 0.0;
				case TBool:
					cs.NativeArray.arraycopy(__boolArray, idx + 1, __boolArray, idx, length - idx - 1);
					__boolArray[--length] = false;
				case TObject, Unknown:
					cs.NativeArray.arraycopy(__objectArray, idx + 1, __objectArray, idx, length - idx - 1);
					__objectArray[--length] = null;
			}
			return true;
		}
		return false;
	}

	public function contains(x:T):Bool {
		return indexOf(x) >= 0;
	}

	public function indexOf(x:T, ?fromIndex:Int):Int {
		var i:Int = (fromIndex == null) ? 0 : fromIndex;
		if (i < 0) {
			i += length;
			if (i < 0)
				i = 0;
		}
		switch (__arrayType) {
			case TInt:
				var xInt:Int = cast x;
				while (i < length) {
					if (__intArray[i] == xInt)
						return i;
					i++;
				}
			case TFloat:
				var xFloat:Float = cast x;
				while (i < length) {
					if (__floatArray[i] == xFloat)
						return i;
					i++;
				}
			case TBool:
				var xBool:Bool = cast x;
				while (i < length) {
					if (__boolArray[i] == xBool)
						return i;
					i++;
				}
			case TObject, Unknown:
				while (i < length) {
					if (__objectArray[i] == x)
						return i;
					i++;
				}
		}
		return -1;
	}

	public function lastIndexOf(x:T, ?fromIndex:Int):Int {
		var i:Int = (fromIndex == null) ? length - 1 : fromIndex;
		if (i >= length)
			i = length - 1;
		else if (i < 0)
			i += length;
		switch (__arrayType) {
			case TInt:
				var xInt:Int = cast x;
				while (i >= 0) {
					if (__intArray[i] == xInt)
						return i;
					i--;
				}
			case TFloat:
				var xFloat:Float = cast x;
				while (i >= 0) {
					if (__floatArray[i] == xFloat)
						return i;
					i--;
				}
			case TBool:
				var xBool:Bool = cast x;
				while (i >= 0) {
					if (__boolArray[i] == xBool)
						return i;
					i--;
				}
			case TObject, Unknown:
				while (i >= 0) {
					if (__objectArray[i] == x)
						return i;
					i--;
				}
		}
		return -1;
	}

	public function copy():Array<T> {
		switch (__arrayType) {
			case TInt:
				var newarr = new cs.NativeArray<Int>(length);
				cs.NativeArray.arraycopy(__intArray, 0, newarr, 0, length);
				return cast __ofIntLiteral(newarr);
			case TFloat:
				var newarr = new cs.NativeArray<Float>(length);
				cs.NativeArray.arraycopy(__floatArray, 0, newarr, 0, length);
				return cast __ofFloatLiteral(newarr);
			case TBool:
				var newarr = new cs.NativeArray<Bool>(length);
				cs.NativeArray.arraycopy(__boolArray, 0, newarr, 0, length);
				return cast __ofBoolLiteral(newarr);
			default:
				var newarr = new cs.NativeArray<Dynamic>(length);
				cs.NativeArray.arraycopy(__objectArray, 0, newarr, 0, length);
				return ofNative(newarr);
		}
	}

	public inline function iterator():haxe.iterators.ArrayIterator<T> {
		return new haxe.iterators.ArrayIterator<T>(this);
	}

	public inline function keyValueIterator():ArrayKeyValueIterator<T> {
		return new ArrayKeyValueIterator<T>(this);
	}

	public function resize(len:Int):Void {
		if (len == 0) {
			length = 0;
			// Reset to appropriate type's empty array
			switch (__arrayType) {
				case TInt:
					__intArray = new cs.NativeArray<Int>(0);
				case TFloat:
					__floatArray = new cs.NativeArray<Float>(0);
				case TBool:
					__boolArray = new cs.NativeArray<Bool>(0);
				default:
					__objectArray = new cs.NativeArray<Dynamic>(0);
			}
		} else if (length < len) {
			// Grow array
			switch (__arrayType) {
				case TInt:
					if (__intArray == null || __intArray.length < len) {
						var newArr = new cs.NativeArray<Int>(len);
						if (__intArray != null && length > 0)
							cs.NativeArray.arraycopy(__intArray, 0, newArr, 0, length);
						__intArray = newArr;
					}
				case TFloat:
					if (__floatArray == null || __floatArray.length < len) {
						var newArr = new cs.NativeArray<Float>(len);
						if (__floatArray != null && length > 0)
							cs.NativeArray.arraycopy(__floatArray, 0, newArr, 0, length);
						__floatArray = newArr;
					}
				case TBool:
					if (__boolArray == null || __boolArray.length < len) {
						var newArr = new cs.NativeArray<Bool>(len);
						if (__boolArray != null && length > 0)
							cs.NativeArray.arraycopy(__boolArray, 0, newArr, 0, length);
						__boolArray = newArr;
					}
				default:
					if (__objectArray == null || __objectArray.length < len) {
						var newArr = new cs.NativeArray<Dynamic>(len);
						if (__objectArray != null && length > 0)
							cs.NativeArray.arraycopy(__objectArray, 0, newArr, 0, length);
						__objectArray = newArr;
					}
			}
			length = len;
		} else if (length > len) {
			// Shrink array - clear trailing elements
			switch (__arrayType) {
				case TInt:
					for (i in len...length)
						__intArray[i] = 0;
				case TFloat:
					for (i in len...length)
						__floatArray[i] = 0.0;
				case TBool:
					for (i in len...length)
						__boolArray[i] = false;
				default:
					for (i in len...length)
						__objectArray[i] = null;
			}
			length = len;
		}
	}

	public inline function map<S>(f:T->S):Array<S> {
		var ret = alloc(length);
		for (i in 0...length)
			ret.__set(i, f(__get(i)));
		return ret;
	}

	public inline function filter(f:T->Bool):Array<T> {
		var ret = [];
		for (i in 0...length) {
			var elt = __get(i);
			if (f(elt))
				ret.push(elt);
		}
		return ret;
	}

	// =========================================================================
	// Internal Accessor Methods (for backwards compatibility)
	// =========================================================================

	private function __get(idx:Int):T {
		if (idx >= length || idx < 0)
			return null;
		return __getDyn(idx);
	}

	private function __set(idx:Int, v:T):T {
		__setDyn(idx, v);
		return v;
	}

	private inline function __unsafe_get(idx:Int):T {
		return __getDyn(idx);
	}

	private inline function __unsafe_set(idx:Int, val:T):T {
		__setDyn(idx, val);
		return val;
	}

	// =========================================================================
	// Typed Method Variants (called by generated code from gencs.ml)
	// These use the typed backing arrays with switch-based dispatch
	// =========================================================================

	// ----- Int Methods -----
	private function __getInt(idx:Int):Int {
		if (idx >= length || idx < 0)
			return 0;
		switch (__arrayType) {
			case TInt:
				return __intArray[idx];
			case TFloat:
				return Std.int(__floatArray[idx]);
			case TObject, Unknown:
				return cast __objectArray[idx];
			case TBool:
				return __boolArray[idx] ? 1 : 0;
		}
	}

	private function __setInt(idx:Int, v:Int):Int {
		switch (__arrayType) {
			case Unknown:
				// First write determines type - use int array
				__initIntArray(idx);
				__intArray[idx] = v;
				__arrayType = TInt;
			case TInt:
				__ensureIntCapacity(idx);
				__intArray[idx] = v;
			case TFloat:
				__ensureFloatCapacity(idx);
				__floatArray[idx] = v;
			case TBool:
				// Bool + Int -> Object
				__upgradeToObject();
				__ensureObjectCapacity(idx);
				__objectArray[idx] = v;
			case TObject:
				__ensureObjectCapacity(idx);
				__objectArray[idx] = v;
		}
		if (idx >= length)
			length = idx + 1;
		return v;
	}

	private function __pushInt(v:Int):Int {
		switch (__arrayType) {
			case Unknown:
				__initIntArray(length);
				__intArray[length] = v;
				__arrayType = TInt;
			case TInt:
				__ensureIntCapacity(length);
				__intArray[length] = v;
			case TFloat:
				__ensureFloatCapacity(length);
				__floatArray[length] = v;
			case TBool:
				__upgradeToObject();
				__ensureObjectCapacity(length);
				__objectArray[length] = v;
			case TObject:
				__ensureObjectCapacity(length);
				__objectArray[length] = v;
		}
		return ++length;
	}

	private function __popInt():Null<Int> {
		if (length == 0)
			return null;
		--length;
		switch (__arrayType) {
			case TInt:
				var v = __intArray[length];
				__intArray[length] = 0;
				return v;
			case TFloat:
				var v = Std.int(__floatArray[length]);
				__floatArray[length] = 0.0;
				return v;
			case TObject, Unknown:
				var v:Int = cast __objectArray[length];
				__objectArray[length] = null;
				return v;
			case TBool:
				var v = __boolArray[length] ? 1 : 0;
				__boolArray[length] = false;
				return v;
		}
	}

	private function __shiftInt():Null<Int> {
		if (length == 0)
			return null;
		var x:Int;
		switch (__arrayType) {
			case TInt:
				x = __intArray[0];
				cs.NativeArray.arraycopy(__intArray, 1, __intArray, 0, length - 1);
				__intArray[length - 1] = 0;
			case TFloat:
				x = Std.int(__floatArray[0]);
				cs.NativeArray.arraycopy(__floatArray, 1, __floatArray, 0, length - 1);
				__floatArray[length - 1] = 0.0;
			case TObject, Unknown:
				x = cast __objectArray[0];
				cs.NativeArray.arraycopy(__objectArray, 1, __objectArray, 0, length - 1);
				__objectArray[length - 1] = null;
			case TBool:
				x = __boolArray[0] ? 1 : 0;
				cs.NativeArray.arraycopy(__boolArray, 1, __boolArray, 0, length - 1);
				__boolArray[length - 1] = false;
		}
		--length;
		return x;
	}

	// ----- Float Methods -----
	private function __getFloat(idx:Int):Float {
		if (idx >= length || idx < 0)
			return 0.0;
		switch (__arrayType) {
			case TFloat:
				return __floatArray[idx];
			case TInt:
				return __intArray[idx];
			case TObject, Unknown:
				return cast __objectArray[idx];
			case TBool:
				return __boolArray[idx] ? 1.0 : 0.0;
		}
	}

	private function __setFloat(idx:Int, v:Float):Float {
		switch (__arrayType) {
			case Unknown:
				__initFloatArray(idx);
				__floatArray[idx] = v;
				__arrayType = TFloat;
			case TFloat:
				__ensureFloatCapacity(idx);
				__floatArray[idx] = v;
			case TInt:
				// Int + Float -> Float (upgrade)
				__upgradeIntToFloat();
				__ensureFloatCapacity(idx);
				__floatArray[idx] = v;
			case TBool:
				// Bool + Float -> Object
				__upgradeToObject();
				__ensureObjectCapacity(idx);
				__objectArray[idx] = v;
			case TObject:
				__ensureObjectCapacity(idx);
				__objectArray[idx] = v;
		}
		if (idx >= length)
			length = idx + 1;
		return v;
	}

	private function __pushFloat(v:Float):Int {
		switch (__arrayType) {
			case Unknown:
				__initFloatArray(length);
				__floatArray[length] = v;
				__arrayType = TFloat;
			case TFloat:
				__ensureFloatCapacity(length);
				__floatArray[length] = v;
			case TInt:
				__upgradeIntToFloat();
				__ensureFloatCapacity(length);
				__floatArray[length] = v;
			case TBool:
				__upgradeToObject();
				__ensureObjectCapacity(length);
				__objectArray[length] = v;
			case TObject:
				__ensureObjectCapacity(length);
				__objectArray[length] = v;
		}
		return ++length;
	}

	private function __popFloat():Null<Float> {
		if (length == 0)
			return null;
		--length;
		switch (__arrayType) {
			case TFloat:
				var v = __floatArray[length];
				__floatArray[length] = 0.0;
				return v;
			case TInt:
				var v:Float = __intArray[length];
				__intArray[length] = 0;
				return v;
			case TObject, Unknown:
				var v:Float = cast __objectArray[length];
				__objectArray[length] = null;
				return v;
			case TBool:
				var v:Float = __boolArray[length] ? 1.0 : 0.0;
				__boolArray[length] = false;
				return v;
		}
	}

	private function __shiftFloat():Null<Float> {
		if (length == 0)
			return null;
		var x:Float;
		switch (__arrayType) {
			case TFloat:
				x = __floatArray[0];
				cs.NativeArray.arraycopy(__floatArray, 1, __floatArray, 0, length - 1);
				__floatArray[length - 1] = 0.0;
			case TInt:
				x = __intArray[0];
				cs.NativeArray.arraycopy(__intArray, 1, __intArray, 0, length - 1);
				__intArray[length - 1] = 0;
			case TObject, Unknown:
				x = cast __objectArray[0];
				cs.NativeArray.arraycopy(__objectArray, 1, __objectArray, 0, length - 1);
				__objectArray[length - 1] = null;
			case TBool:
				x = __boolArray[0] ? 1.0 : 0.0;
				cs.NativeArray.arraycopy(__boolArray, 1, __boolArray, 0, length - 1);
				__boolArray[length - 1] = false;
		}
		--length;
		return x;
	}

	// ----- Bool Methods -----
	private function __getBool(idx:Int):Bool {
		if (idx >= length || idx < 0)
			return false;
		switch (__arrayType) {
			case TBool:
				return __boolArray[idx];
			case TInt:
				return __intArray[idx] != 0;
			case TFloat:
				return __floatArray[idx] != 0.0;
			case TObject, Unknown:
				return cast __objectArray[idx];
		}
	}

	private function __setBool(idx:Int, v:Bool):Bool {
		switch (__arrayType) {
			case Unknown:
				__initBoolArray(idx);
				__boolArray[idx] = v;
				__arrayType = TBool;
			case TBool:
				__ensureBoolCapacity(idx);
				__boolArray[idx] = v;
			case TInt, TFloat:
				// Int/Float + Bool -> Object
				__upgradeToObject();
				__ensureObjectCapacity(idx);
				__objectArray[idx] = v;
			case TObject:
				__ensureObjectCapacity(idx);
				__objectArray[idx] = v;
		}
		if (idx >= length)
			length = idx + 1;
		return v;
	}

	private function __pushBool(v:Bool):Int {
		switch (__arrayType) {
			case Unknown:
				__initBoolArray(length);
				__boolArray[length] = v;
				__arrayType = TBool;
			case TBool:
				__ensureBoolCapacity(length);
				__boolArray[length] = v;
			case TInt, TFloat:
				__upgradeToObject();
				__ensureObjectCapacity(length);
				__objectArray[length] = v;
			case TObject:
				__ensureObjectCapacity(length);
				__objectArray[length] = v;
		}
		return ++length;
	}

	private function __setObject(idx:Int, v:Dynamic):Dynamic {
		// Object set - upgrades any primitive storage to object
		if (__arrayType != TObject) {
			if (__arrayType == Unknown) {
				__initObjectArray(idx);
			} else {
				__upgradeToObject();
			}
			__arrayType = TObject;
		}
		__ensureObjectCapacity(idx);
		__objectArray[idx] = v;
		if (idx >= length)
			length = idx + 1;
		return v;
	}

	private function __popBool():Null<Bool> {
		if (length == 0)
			return null;
		--length;
		switch (__arrayType) {
			case TBool:
				var v = __boolArray[length];
				__boolArray[length] = false;
				return v;
			case TInt:
				var v = __intArray[length] != 0;
				__intArray[length] = 0;
				return v;
			case TFloat:
				var v = __floatArray[length] != 0.0;
				__floatArray[length] = 0.0;
				return v;
			case TObject, Unknown:
				var v:Bool = cast __objectArray[length];
				__objectArray[length] = null;
				return v;
		}
	}

	private function __shiftBool():Null<Bool> {
		if (length == 0)
			return null;
		var x:Bool;
		switch (__arrayType) {
			case TBool:
				x = __boolArray[0];
				cs.NativeArray.arraycopy(__boolArray, 1, __boolArray, 0, length - 1);
				__boolArray[length - 1] = false;
			case TInt:
				x = __intArray[0] != 0;
				cs.NativeArray.arraycopy(__intArray, 1, __intArray, 0, length - 1);
				__intArray[length - 1] = 0;
			case TFloat:
				x = __floatArray[0] != 0.0;
				cs.NativeArray.arraycopy(__floatArray, 1, __floatArray, 0, length - 1);
				__floatArray[length - 1] = 0.0;
			case TObject, Unknown:
				x = cast __objectArray[0];
				cs.NativeArray.arraycopy(__objectArray, 1, __objectArray, 0, length - 1);
				__objectArray[length - 1] = null;
		}
		--length;
		return x;
	}

	// ----- Dynamic/Object Methods -----
	private function __getDyn(idx:Int):Dynamic {
		if (idx >= length || idx < 0)
			return null;
		switch (__arrayType) {
			case TObject, Unknown:
				return __objectArray[idx];
			case TInt:
				return __intArray[idx];
			case TFloat:
				return __floatArray[idx];
			case TBool:
				return __boolArray[idx];
		}
	}

	private function __setDyn(idx:Int, v:Dynamic):Dynamic {
		// If locked, we must write to the existing typed array (no upgrade allowed)
		// This preserves the shared storage for arrays created via __ofIntLiteral etc.
		if (__locked) {
			switch (__arrayType) {
				case TInt:
					__ensureIntCapacity(idx);
					__intArray[idx] = untyped __cs__("haxe.lang.Runtime.toInt({0})", v);
				case TFloat:
					__ensureFloatCapacity(idx);
					__floatArray[idx] = untyped __cs__("haxe.lang.Runtime.toDouble({0})", v);
				case TBool:
					__ensureBoolCapacity(idx);
					__boolArray[idx] = untyped __cs__("haxe.lang.Runtime.toBool({0})", v);
				case TObject:
					__ensureObjectCapacity(idx);
					__objectArray[idx] = v;
				case Unknown:
					// Shouldn't happen for locked arrays, but handle it
					__initObjectArray(idx);
					__arrayType = TObject;
					__objectArray[idx] = v;
			}
		} else {
			// Not locked - upgrade to object (existing behavior)
			if (__arrayType != TObject) {
				if (__arrayType == Unknown) {
					__initObjectArray(idx);
					__arrayType = TObject;
				} else {
					__upgradeToObject();
				}
			}
			__ensureObjectCapacity(idx);
			__objectArray[idx] = v;
		}
		if (idx >= length)
			length = idx + 1;
		return v;
	}

	private function __pushDyn(v:Dynamic):Int {
		// If locked, we must write to the existing typed array (no upgrade allowed)
		if (__locked) {
			switch (__arrayType) {
				case TInt:
					__ensureIntCapacity(length);
					__intArray[length] = untyped __cs__("haxe.lang.Runtime.toInt({0})", v);
				case TFloat:
					__ensureFloatCapacity(length);
					__floatArray[length] = untyped __cs__("haxe.lang.Runtime.toDouble({0})", v);
				case TBool:
					__ensureBoolCapacity(length);
					__boolArray[length] = untyped __cs__("haxe.lang.Runtime.toBool({0})", v);
				case TObject:
					__ensureObjectCapacity(length);
					__objectArray[length] = v;
				case Unknown:
					__initObjectArray(length);
					__arrayType = TObject;
					__objectArray[length] = v;
			}
		} else {
			// Not locked - upgrade to object (existing behavior)
			if (__arrayType != TObject) {
				if (__arrayType == Unknown) {
					__initObjectArray(length);
					__arrayType = TObject;
				} else {
					__upgradeToObject();
				}
			}
			__ensureObjectCapacity(length);
			__objectArray[length] = v;
		}
		return ++length;
	}

	// ----- Unshift Methods -----
	private function __unshiftInt(v:Int):Void {
		switch (__arrayType) {
			case Unknown:
				__initIntArray(length);
				__intArray[0] = v;
				__arrayType = TInt;
			case TInt:
				if (__intArray == null || length >= __intArray.length) {
					var newLen = (length << 1) + 1;
					if (newLen < __hx_defaultCapacity)
						newLen = __hx_defaultCapacity;
					var newarr = new cs.NativeArray<Int>(newLen);
					if (__intArray != null)
						cs.NativeArray.arraycopy(__intArray, 0, newarr, 1, length);
					__intArray = newarr;
				} else {
					cs.NativeArray.arraycopy(__intArray, 0, __intArray, 1, length);
				}
				__intArray[0] = v;
			case TFloat:
				if (__floatArray == null || length >= __floatArray.length) {
					var newLen = (length << 1) + 1;
					if (newLen < __hx_defaultCapacity)
						newLen = __hx_defaultCapacity;
					var newarr = new cs.NativeArray<Float>(newLen);
					if (__floatArray != null)
						cs.NativeArray.arraycopy(__floatArray, 0, newarr, 1, length);
					__floatArray = newarr;
				} else {
					cs.NativeArray.arraycopy(__floatArray, 0, __floatArray, 1, length);
				}
				__floatArray[0] = v;
			case TBool:
				__upgradeToObject();
				__unshiftToObject(v);
			case TObject:
				__unshiftToObject(v);
		}
		++length;
	}

	private function __unshiftFloat(v:Float):Void {
		switch (__arrayType) {
			case Unknown:
				__initFloatArray(length);
				__floatArray[0] = v;
				__arrayType = TFloat;
			case TFloat:
				if (__floatArray == null || length >= __floatArray.length) {
					var newLen = (length << 1) + 1;
					if (newLen < __hx_defaultCapacity)
						newLen = __hx_defaultCapacity;
					var newarr = new cs.NativeArray<Float>(newLen);
					if (__floatArray != null)
						cs.NativeArray.arraycopy(__floatArray, 0, newarr, 1, length);
					__floatArray = newarr;
				} else {
					cs.NativeArray.arraycopy(__floatArray, 0, __floatArray, 1, length);
				}
				__floatArray[0] = v;
			case TInt:
				__upgradeIntToFloat();
				if (__floatArray == null || length >= __floatArray.length) {
					var newLen = (length << 1) + 1;
					if (newLen < __hx_defaultCapacity)
						newLen = __hx_defaultCapacity;
					var newarr = new cs.NativeArray<Float>(newLen);
					if (__floatArray != null)
						cs.NativeArray.arraycopy(__floatArray, 0, newarr, 1, length);
					__floatArray = newarr;
				} else {
					cs.NativeArray.arraycopy(__floatArray, 0, __floatArray, 1, length);
				}
				__floatArray[0] = v;
			case TBool:
				__upgradeToObject();
				__unshiftToObject(v);
			case TObject:
				__unshiftToObject(v);
		}
		++length;
	}

	private function __unshiftBool(v:Bool):Void {
		switch (__arrayType) {
			case Unknown:
				__initBoolArray(length);
				__boolArray[0] = v;
				__arrayType = TBool;
			case TBool:
				if (__boolArray == null || length >= __boolArray.length) {
					var newLen = (length << 1) + 1;
					if (newLen < __hx_defaultCapacity)
						newLen = __hx_defaultCapacity;
					var newarr = new cs.NativeArray<Bool>(newLen);
					if (__boolArray != null)
						cs.NativeArray.arraycopy(__boolArray, 0, newarr, 1, length);
					__boolArray = newarr;
				} else {
					cs.NativeArray.arraycopy(__boolArray, 0, __boolArray, 1, length);
				}
				__boolArray[0] = v;
			case TInt, TFloat:
				__upgradeToObject();
				__unshiftToObject(v);
			case TObject:
				__unshiftToObject(v);
		}
		++length;
	}

	private function __unshiftDyn(v:Dynamic):Void {
		if (__arrayType != TObject) {
			if (__arrayType == Unknown) {
				__initObjectArray(length);
				__arrayType = TObject;
			} else {
				__upgradeToObject();
			}
		}
		__unshiftToObject(v);
		++length;
	}

	private function __unshiftToObject(v:Dynamic):Void {
		if (length >= __objectArray.length) {
			var newLen = (length << 1) + 1;
			var newarr = new cs.NativeArray<Dynamic>(newLen);
			cs.NativeArray.arraycopy(__objectArray, 0, newarr, 1, length);
			__objectArray = newarr;
		} else {
			cs.NativeArray.arraycopy(__objectArray, 0, __objectArray, 1, length);
		}
		__objectArray[0] = v;
	}

	// ----- Insert Methods -----
	private function __insertInt(pos:Int, v:Int):Void {
		if (pos < 0) {
			pos = length + pos;
			if (pos < 0)
				pos = 0;
		}
		if (pos >= length) {
			__pushInt(v);  // Append at end, don't create sparse array
			return;
		} else if (pos == 0) {
			__unshiftInt(v);
			return;
		}
		switch (__arrayType) {
			case Unknown:
				__initIntArray(length);
				__intArray[pos] = v;
				__arrayType = TInt;
			case TInt:
				if (__intArray == null || length >= __intArray.length) {
					var newLen = (length << 1) + 1;
					if (newLen < __hx_defaultCapacity)
						newLen = __hx_defaultCapacity;
					var newarr = new cs.NativeArray<Int>(newLen);
					cs.NativeArray.arraycopy(__intArray, 0, newarr, 0, pos);
					newarr[pos] = v;
					cs.NativeArray.arraycopy(__intArray, pos, newarr, pos + 1, length - pos);
					__intArray = newarr;
				} else {
					cs.NativeArray.arraycopy(__intArray, pos, __intArray, pos + 1, length - pos);
					__intArray[pos] = v;
				}
			case TFloat:
				if (__floatArray == null || length >= __floatArray.length) {
					var newLen = (length << 1) + 1;
					if (newLen < __hx_defaultCapacity)
						newLen = __hx_defaultCapacity;
					var newarr = new cs.NativeArray<Float>(newLen);
					cs.NativeArray.arraycopy(__floatArray, 0, newarr, 0, pos);
					newarr[pos] = v;
					cs.NativeArray.arraycopy(__floatArray, pos, newarr, pos + 1, length - pos);
					__floatArray = newarr;
				} else {
					cs.NativeArray.arraycopy(__floatArray, pos, __floatArray, pos + 1, length - pos);
					__floatArray[pos] = v;
				}
			case TBool:
				__upgradeToObject();
				__insertToObject(pos, v);
			case TObject:
				__insertToObject(pos, v);
		}
		++length;
	}

	private function __insertFloat(pos:Int, v:Float):Void {
		if (pos < 0) {
			pos = length + pos;
			if (pos < 0)
				pos = 0;
		}
		if (pos >= length) {
			__pushFloat(v);
			return;
		} else if (pos == 0) {
			__unshiftFloat(v);
			return;
		}
		switch (__arrayType) {
			case Unknown:
				__initFloatArray(length);
				__floatArray[pos] = v;
				__arrayType = TFloat;
			case TFloat:
				if (__floatArray == null || length >= __floatArray.length) {
					var newLen = (length << 1) + 1;
					if (newLen < __hx_defaultCapacity)
						newLen = __hx_defaultCapacity;
					var newarr = new cs.NativeArray<Float>(newLen);
					cs.NativeArray.arraycopy(__floatArray, 0, newarr, 0, pos);
					newarr[pos] = v;
					cs.NativeArray.arraycopy(__floatArray, pos, newarr, pos + 1, length - pos);
					__floatArray = newarr;
				} else {
					cs.NativeArray.arraycopy(__floatArray, pos, __floatArray, pos + 1, length - pos);
					__floatArray[pos] = v;
				}
			case TInt:
				__upgradeIntToFloat();
				if (__floatArray == null || length >= __floatArray.length) {
					var newLen = (length << 1) + 1;
					if (newLen < __hx_defaultCapacity)
						newLen = __hx_defaultCapacity;
					var newarr = new cs.NativeArray<Float>(newLen);
					cs.NativeArray.arraycopy(__floatArray, 0, newarr, 0, pos);
					newarr[pos] = v;
					cs.NativeArray.arraycopy(__floatArray, pos, newarr, pos + 1, length - pos);
					__floatArray = newarr;
				} else {
					cs.NativeArray.arraycopy(__floatArray, pos, __floatArray, pos + 1, length - pos);
					__floatArray[pos] = v;
				}
			case TBool:
				__upgradeToObject();
				__insertToObject(pos, v);
			case TObject:
				__insertToObject(pos, v);
		}
		++length;
	}

	private function __insertBool(pos:Int, v:Bool):Void {
		if (pos < 0) {
			pos = length + pos;
			if (pos < 0)
				pos = 0;
		}
		if (pos >= length) {
			__pushBool(v);
			return;
		} else if (pos == 0) {
			__unshiftBool(v);
			return;
		}
		switch (__arrayType) {
			case Unknown:
				__initBoolArray(length);
				__boolArray[pos] = v;
				__arrayType = TBool;
			case TBool:
				if (__boolArray == null || length >= __boolArray.length) {
					var newLen = (length << 1) + 1;
					if (newLen < __hx_defaultCapacity)
						newLen = __hx_defaultCapacity;
					var newarr = new cs.NativeArray<Bool>(newLen);
					cs.NativeArray.arraycopy(__boolArray, 0, newarr, 0, pos);
					newarr[pos] = v;
					cs.NativeArray.arraycopy(__boolArray, pos, newarr, pos + 1, length - pos);
					__boolArray = newarr;
				} else {
					cs.NativeArray.arraycopy(__boolArray, pos, __boolArray, pos + 1, length - pos);
					__boolArray[pos] = v;
				}
			case TInt, TFloat:
				__upgradeToObject();
				__insertToObject(pos, v);
			case TObject:
				__insertToObject(pos, v);
		}
		++length;
	}

	private function __insertDyn(pos:Int, v:Dynamic):Void {
		if (pos < 0) {
			pos = length + pos;
			if (pos < 0)
				pos = 0;
		}
		if (pos >= length) {
			__pushDyn(v);
			return;
		} else if (pos == 0) {
			__unshiftDyn(v);
			return;
		}
		if (__arrayType != TObject) {
			if (__arrayType == Unknown) {
				__initObjectArray(length);
				__arrayType = TObject;
			} else {
				__upgradeToObject();
			}
		}
		__insertToObject(pos, v);
		++length;
	}

	private function __insertToObject(pos:Int, v:Dynamic):Void {
		if (length >= __objectArray.length) {
			var newLen = (length << 1) + 1;
			var newarr = new cs.NativeArray<Dynamic>(newLen);
			cs.NativeArray.arraycopy(__objectArray, 0, newarr, 0, pos);
			newarr[pos] = v;
			cs.NativeArray.arraycopy(__objectArray, pos, newarr, pos + 1, length - pos);
			__objectArray = newarr;
		} else {
			cs.NativeArray.arraycopy(__objectArray, pos, __objectArray, pos + 1, length - pos);
			__objectArray[pos] = v;
		}
	}

	// =========================================================================
	// Array Initialization and Capacity Management
	// =========================================================================

	private function __initIntArray(minCapacity:Int):Void {
		var cap = minCapacity + 1;
		if (cap < __hx_defaultCapacity)
			cap = __hx_defaultCapacity;
		__intArray = new cs.NativeArray<Int>(cap);
	}

	private function __initFloatArray(minCapacity:Int):Void {
		var cap = minCapacity + 1;
		if (cap < __hx_defaultCapacity)
			cap = __hx_defaultCapacity;
		__floatArray = new cs.NativeArray<Float>(cap);
	}

	private function __initBoolArray(minCapacity:Int):Void {
		var cap = minCapacity + 1;
		if (cap < __hx_defaultCapacity)
			cap = __hx_defaultCapacity;
		__boolArray = new cs.NativeArray<Bool>(cap);
	}

	private function __initObjectArray(minCapacity:Int):Void {
		var cap = minCapacity + 1;
		if (cap < __hx_defaultCapacity)
			cap = __hx_defaultCapacity;
		__objectArray = new cs.NativeArray<Dynamic>(cap);
	}

	private function __ensureIntCapacity(idx:Int):Void {
		if (__intArray == null || idx >= __intArray.length) {
			var newLen = idx + 1;
			if (__intArray != null && idx == __intArray.length)
				newLen = (idx << 1) + 1;
			if (newLen < __hx_defaultCapacity)
				newLen = __hx_defaultCapacity;
			var newArr = new cs.NativeArray<Int>(newLen);
			if (__intArray != null && length > 0)
				cs.NativeArray.arraycopy(__intArray, 0, newArr, 0, length);
			__intArray = newArr;
		}
	}

	private function __ensureFloatCapacity(idx:Int):Void {
		if (__floatArray == null || idx >= __floatArray.length) {
			var newLen = idx + 1;
			if (__floatArray != null && idx == __floatArray.length)
				newLen = (idx << 1) + 1;
			if (newLen < __hx_defaultCapacity)
				newLen = __hx_defaultCapacity;
			var newArr = new cs.NativeArray<Float>(newLen);
			if (__floatArray != null && length > 0)
				cs.NativeArray.arraycopy(__floatArray, 0, newArr, 0, length);
			__floatArray = newArr;
		}
	}

	private function __ensureBoolCapacity(idx:Int):Void {
		if (__boolArray == null || idx >= __boolArray.length) {
			var newLen = idx + 1;
			if (__boolArray != null && idx == __boolArray.length)
				newLen = (idx << 1) + 1;
			if (newLen < __hx_defaultCapacity)
				newLen = __hx_defaultCapacity;
			var newArr = new cs.NativeArray<Bool>(newLen);
			if (__boolArray != null && length > 0)
				cs.NativeArray.arraycopy(__boolArray, 0, newArr, 0, length);
			__boolArray = newArr;
		}
	}

	private function __ensureObjectCapacity(idx:Int):Void {
		if (__objectArray == null || idx >= __objectArray.length) {
			var newLen = idx + 1;
			if (__objectArray != null && idx == __objectArray.length)
				newLen = (idx << 1) + 1;
			if (newLen < __hx_defaultCapacity)
				newLen = __hx_defaultCapacity;
			var newArr = new cs.NativeArray<Dynamic>(newLen);
			if (__objectArray != null && length > 0)
				cs.NativeArray.arraycopy(__objectArray, 0, newArr, 0, length);
			__objectArray = newArr;
		}
	}

	// =========================================================================
	// Type Upgrade Methods (never downgrade)
	// =========================================================================

	private function __upgradeIntToFloat():Void {
		__ensureFloatCapacity(length > 0 ? length - 1 : 0);
		if (__intArray != null) {
			for (i in 0...length)
				__floatArray[i] = __intArray[i];
		}
		__intArray = null;
		__arrayType = TFloat;
	}

	private function __upgradeToObject():Void {
		// Create new object array with enough capacity
		// Do NOT use __ensureObjectCapacity here because it would try to copy from __objectArray
		// which doesn't have the data yet - data is in the typed arrays
		var cap = length > 0 ? length : __hx_defaultCapacity;
		if (cap < __hx_defaultCapacity)
			cap = __hx_defaultCapacity;
		var newObjectArray = new cs.NativeArray<Dynamic>(cap);

		switch (__arrayType) {
			case TInt:
				if (__intArray != null) {
					for (i in 0...length)
						newObjectArray[i] = __intArray[i];
				}
				__intArray = null;
			case TFloat:
				if (__floatArray != null) {
					for (i in 0...length)
						newObjectArray[i] = __floatArray[i];
				}
				__floatArray = null;
			case TBool:
				if (__boolArray != null) {
					for (i in 0...length)
						newObjectArray[i] = __boolArray[i];
				}
				__boolArray = null;
			case TObject, Unknown:
				// Already object or unknown - keep existing object array if any
				if (__objectArray != null && __objectArray.length >= cap) {
					// Already have adequate object array
					__arrayType = TObject;
					return;
				}
				// Copy from existing object array if there's data
				if (__objectArray != null && length > 0) {
					for (i in 0...length)
						newObjectArray[i] = __objectArray[i];
				}
		}
		__objectArray = newObjectArray;
		__arrayType = TObject;
	}

	// =========================================================================
	// Locking and Casting Methods (for type covariance)
	// =========================================================================

	/**
	 * Cast array to a specific storage type and lock it.
	 * Called by generated code for array casts.
	 *
	 * targetType: 1=Int, 2=Float, 3=Bool, 4=Object
	 *
	 * Locking rules:
	 * - If already locked to matching type: return this
	 * - If already locked to different type: throw InvalidCastException
	 * - If unlocked: migrate data to target storage, lock
	 */
	private function __cast(targetType:Int):Array<T> {
		if (__locked) {
			// Already locked - verify compatible
			var currentType:Int = cast __arrayType;
			if (currentType != targetType && targetType != 4) {
				// targetType 4 (Object) is always compatible
				throw "InvalidCastException: Cannot cast locked array to different type";
			}
			return this;
		}

		// Not locked - migrate and lock
		switch (targetType) {
			case 1:
				__migrateToInt();
			case 2:
				__migrateToFloat();
			case 3:
				__migrateToBool();
			case 4:
				__migrateToObject();
		}
		__locked = true;
		return this;
	}

	/**
	 * Migrate storage to int array.
	 * Only callable when unlocked.
	 */
	private function __migrateToInt():Void {
		switch (__arrayType) {
			case Unknown:
				__initIntArray(length > 0 ? length - 1 : 0);
			case TInt:
				// Already int storage
			case TFloat:
				// Float -> Int: truncate values
				__initIntArray(length > 0 ? length - 1 : 0);
				if (__floatArray != null) {
					for (i in 0...length)
						__intArray[i] = Std.int(__floatArray[i]);
				}
				__floatArray = null;
			case TBool:
				// Bool -> Int: convert
				__initIntArray(length > 0 ? length - 1 : 0);
				if (__boolArray != null) {
					for (i in 0...length)
						__intArray[i] = __boolArray[i] ? 1 : 0;
				}
				__boolArray = null;
			case TObject:
				// Object -> Int: cast each element
				__initIntArray(length > 0 ? length - 1 : 0);
				if (__objectArray != null) {
					for (i in 0...length)
						__intArray[i] = cast __objectArray[i];
				}
				__objectArray = null;
		}
		__arrayType = TInt;
	}

	/**
	 * Migrate storage to float array.
	 * Only callable when unlocked.
	 */
	private function __migrateToFloat():Void {
		switch (__arrayType) {
			case Unknown:
				__initFloatArray(length > 0 ? length - 1 : 0);
			case TInt:
				// Int -> Float: widen values
				__initFloatArray(length > 0 ? length - 1 : 0);
				if (__intArray != null) {
					for (i in 0...length)
						__floatArray[i] = __intArray[i];
				}
				__intArray = null;
			case TFloat:
				// Already float storage
			case TBool:
				// Bool -> Float: convert
				__initFloatArray(length > 0 ? length - 1 : 0);
				if (__boolArray != null) {
					for (i in 0...length)
						__floatArray[i] = __boolArray[i] ? 1.0 : 0.0;
				}
				__boolArray = null;
			case TObject:
				// Object -> Float: cast each element
				__initFloatArray(length > 0 ? length - 1 : 0);
				if (__objectArray != null) {
					for (i in 0...length)
						__floatArray[i] = cast __objectArray[i];
				}
				__objectArray = null;
		}
		__arrayType = TFloat;
	}

	/**
	 * Migrate storage to bool array.
	 * Only callable when unlocked.
	 */
	private function __migrateToBool():Void {
		switch (__arrayType) {
			case Unknown:
				__initBoolArray(length > 0 ? length - 1 : 0);
			case TInt:
				// Int -> Bool: non-zero = true
				__initBoolArray(length > 0 ? length - 1 : 0);
				if (__intArray != null) {
					for (i in 0...length)
						__boolArray[i] = __intArray[i] != 0;
				}
				__intArray = null;
			case TFloat:
				// Float -> Bool: non-zero = true
				__initBoolArray(length > 0 ? length - 1 : 0);
				if (__floatArray != null) {
					for (i in 0...length)
						__boolArray[i] = __floatArray[i] != 0.0;
				}
				__floatArray = null;
			case TBool:
				// Already bool storage
			case TObject:
				// Object -> Bool: cast each element
				__initBoolArray(length > 0 ? length - 1 : 0);
				if (__objectArray != null) {
					for (i in 0...length)
						__boolArray[i] = cast __objectArray[i];
				}
				__objectArray = null;
		}
		__arrayType = TBool;
	}

	/**
	 * Migrate storage to object array.
	 * Only callable when unlocked.
	 */
	private function __migrateToObject():Void {
		__upgradeToObject();
	}

	// =========================================================================
	// Factory Methods (for array literals)
	// =========================================================================

	/**
	 * Create a locked int array from a native int array.
	 * Called by generated code for int array literals.
	 */
	private static function __ofIntLiteral(native:cs.NativeArray<Int>):Array<Int> {
		var a = new Array<Int>();
		a.length = native.length;
		a.__intArray = native;
		a.__arrayType = TInt;
		a.__locked = true;
		return a;
	}

	/**
	 * Create a locked float array from a native float array.
	 * Called by generated code for float array literals.
	 */
	private static function __ofFloatLiteral(native:cs.NativeArray<Float>):Array<Float> {
		var a = new Array<Float>();
		a.length = native.length;
		a.__floatArray = native;
		a.__arrayType = TFloat;
		a.__locked = true;
		return a;
	}

	/**
	 * Create a locked bool array from a native bool array.
	 * Called by generated code for bool array literals.
	 */
	private static function __ofBoolLiteral(native:cs.NativeArray<Bool>):Array<Bool> {
		var a = new Array<Bool>();
		a.length = native.length;
		a.__boolArray = native;
		a.__arrayType = TBool;
		a.__locked = true;
		return a;
	}

	/**
	 * Create a locked object array from a native object array.
	 * Called by generated code for object array literals (Array<SomeClass>, Array<String>, etc).
	 * Returns Array<Dynamic> - caller uses System.Runtime.CompilerServices.Unsafe.As to convert.
	 */
	private static function __ofObjectLiteral(native:cs.NativeArray<Dynamic>):Array<Dynamic> {
		var a = new Array<Dynamic>();
		a.length = native.length;
		a.__objectArray = native;
		a.__arrayType = TObject;
		a.__locked = true;
		return a;
	}

	/**
	 * Create an UNLOCKED dynamic array from a native object array.
	 * Called by generated code for Array<Dynamic> literals.
	 * The array is NOT locked, allowing subsequent casts to typed arrays.
	 */
	private static function __ofDynLiteral(native:cs.NativeArray<Dynamic>):Array<Dynamic> {
		var a = new Array<Dynamic>();
		a.length = native.length;
		a.__objectArray = native;
		a.__arrayType = TObject;
		a.__locked = false; // NOT locked for Dynamic
		return a;
	}
}
