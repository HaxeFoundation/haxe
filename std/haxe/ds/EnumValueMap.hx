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

package haxe.ds;

import haxe.Constraints.IMap;
import Type.ValueType;

/**
	EnumValueMap allows mapping of enum value keys to arbitrary values.
**/
class EnumValueMap<K:EnumValue, V> implements IMap<K, V> {
	var enums:Array<K> = [];
	var values:Array<V> = [];

	public function new() {}

	public function get(k:K):Null<V> {
		for(index => e in enums) {
			if(equalEnums(e, k)) {
				return values[index];
			}
		}
		return null;
	}

	public function set(k:K, v:V):Void {
		for(index => e in enums) {
			if(equalEnums(e, k)) {
				values[index] = v;
				return;
			}
		}
		enums.push(k);
		values.push(v);
	}

	public function exists(k:K):Bool {
		for(e in enums) {
			if(equalEnums(e, k)) {
				return true;
			}
		}
		return false;
	}

	public function remove(k:K):Bool {
		for(index => e in enums) {
			if(equalEnums(e, k)) {
				enums.splice(index, 1);
				values.splice(index, 1);
				return true;
			}
		}
		return false;
	}

	public inline function keys():Iterator<K> {
		return enums.iterator();
	}

	public inline function iterator():Iterator<V> {
		return values.iterator();
	}

	public inline function keyValueIterator():KeyValueIterator<K, V> {
		return new haxe.iterators.MapKeyValueIterator(this);
	}

	public function copy():IMap<K, V> {
		var result = new EnumValueMap();
		result.enums = enums.copy();
		result.values = values.copy();
		return result;
	}

	public function toString():String {
		var pairs = [for(i => e in enums) Std.string(e) + ' => ' + Std.string(values[i])];
		return '[' + pairs.join(', ') + ']';
	}

	public function clear():Void {
		enums.resize(0);
		values.resize(0);
	}

	static function equal(v1:Dynamic, v2:Dynamic):Bool {
		if(v1 == v2)
			return true;

		return switch [Type.typeof(v1), Type.typeof(v2)] {
			case [TClass(Array), TClass(Array)]:
				equalArrays(v1, v2);
			case [TEnum(_), TEnum(_)]:
				equalEnums(v1, v2);
			case _:
				false;
		}
	}

	static function equalEnums(e1:EnumValue, e2:EnumValue) {
		if(e1 == e2)
			return true;

		if(Type.enumIndex(e1) != Type.enumIndex(e2))
			return false;

		if(Type.getEnum(e1) != Type.getEnum(e2))
			return false;

		var params1 = Type.enumParameters(e1);
		var params2 = Type.enumParameters(e2);
		if(params1.length != params2.length)
			return false;

		for(i => v1 in params1) {
			if(!equal(v1, params2[i]))
				return false;
		}

		return true;
	}

	static function equalArrays(a1:Array<Dynamic>, a2:Array<Dynamic>):Bool {
		if(a1 == a2)
			return true;

		if(a1.length != a2.length)
			return false;

		for(i => v1 in a1) {
			if(!equal(v1, a2[i]))
				return false;
		}
		return true;
	}
}
