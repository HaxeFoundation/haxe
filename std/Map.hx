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

import haxe.ds.StringMap;
import haxe.ds.IntMap;
import haxe.ds.HashMap;
import haxe.ds.ObjectMap;
import haxe.ds.WeakMap;
import haxe.ds.EnumValueMap;
import haxe.Constraints.IMap;

 /**
	Map allows key to value mapping for arbitrary value types, and many key
	types.

	This is a multi-type abstract, it is instantiated as one of its
	specialization types depending on its type parameters.

	A Map can be instantiated without explicit type parameters. Type inference
	will then determine the type parameters from the usage.

	Maps can also be created with `key1 => value1, key2 => value2` syntax.

	Map is an abstract type, it is not available at runtime.

	@see https://haxe.org/manual/std-Map.html
**/
@:multiType(@:followWithAbstracts K)
abstract Map<K,V>(IMap<K,V> ) {

	/**
		Creates a new Map.

		This becomes a constructor call to one of the specialization types in
		the output. The rules for that are as follows:

		1. if K is a `String`, `haxe.ds.StringMap` is used
		2. if K is an `Int`, `haxe.ds.IntMap` is used
		3. if K is an `EnumValue`, `haxe.ds.EnumValueMap` is used
		4. if K is any other class or structure, `haxe.ds.ObjectMap` is used
		5. if K is any other type, it causes a compile-time error

		(Cpp) Map does not use weak keys on ObjectMap by default.
	**/
	public function new();

	/**
		Maps `key` to `value`.

		If `key` already has a mapping, the previous value disappears.

		If `key` is null, the result is unspecified.
	**/
	public inline function set(key:K, value:V) this.set(key, value);

	/**
		Returns the current mapping of `key`.

		If no such mapping exists, null is returned.

		Note that a check like `map.get(key) == null` can hold for two reasons:

		1. the map has no mapping for `key`
		2. the map has a mapping with a value of `null`

		If it is important to distinguish these cases, `exists()` should be
		used.

		If `key` is null, the result is unspecified.
	**/
	@:arrayAccess public inline function get(key:K) return this.get(key);

	/**
		Returns true if `key` has a mapping, false otherwise.

		If `key` is null, the result is unspecified.
	**/
	public inline function exists(key:K) return this.exists(key);

	/**
		Removes the mapping of `key` and returns true if such a mapping existed,
		false otherwise.

		If `key` is null, the result is unspecified.
	**/
	public inline function remove(key:K) return this.remove(key);

	/**
		Returns an Iterator over the keys of `this` Map.

		The order of keys is undefined.
	**/
	public inline function keys():Iterator<K> {
		return this.keys();
	}

	/**
		Returns an Iterator over the values of `this` Map.

		The order of values is undefined.
	**/
	public inline function iterator():Iterator<V> {
		return this.iterator();
	}

	/**
		Returns a String representation of `this` Map.

		The exact representation depends on the platform and key-type.
	**/
	public inline function toString():String {
		return this.toString();
	}

	@:arrayAccess @:noCompletion public inline function arrayWrite(k:K, v:V):V {
		this.set(k, v);
		return v;
	}

	@:to static inline function toStringMap<K:String,V>(t:IMap<K,V>):StringMap<V> {
		return new StringMap<V>();
	}

	@:to static inline function toIntMap<K:Int,V>(t:IMap<K,V>):IntMap<V> {
		return new IntMap<V>();
	}

	@:to static inline function toEnumValueMapMap<K:EnumValue,V>(t:IMap<K,V>):EnumValueMap<K,V> {
		return new EnumValueMap<K, V>();
	}

	@:to static inline function toObjectMap<K:{ },V>(t:IMap<K,V>):ObjectMap<K,V> {
		return new ObjectMap<K, V>();
	}

	@:from static inline function fromStringMap<V>(map:StringMap<V>):Map< String, V > {
		return cast map;
	}

	@:from static inline function fromIntMap<V>(map:IntMap<V>):Map< Int, V > {
		return cast map;
	}

	@:from static inline function fromObjectMap<K:{ }, V>(map:ObjectMap<K,V>):Map<K,V> {
		return cast map;
	}
}

@:dox(hide)
@:deprecated
typedef IMap<K, V> = haxe.Constraints.IMap<K, V>;