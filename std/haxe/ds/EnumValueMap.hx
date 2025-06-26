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

/**
	EnumValueMap allows mapping of enum value keys to arbitrary values.

	Keys are compared by value and recursively over their parameters. If any
	parameter is not an enum value, `Reflect.compare` is used to compare them.
**/
class EnumValueMap<K:EnumValue, V> extends haxe.ds.BalancedTree<K, V> implements haxe.Constraints.IMap<K, V> {
	var sortIndices = new Map<{}, Int>();
	var sortIndicesCounter = 0;

	override function compare(k1:EnumValue, k2:EnumValue):Int {
		var d = k1.getIndex() - k2.getIndex();
		if (d != 0)
			return d;
		#if hl
		var a1 = @:privateAccess Type._enumParameters(k1);
		var a2 = @:privateAccess Type._enumParameters(k2);
		var ld = a1.length - a2.length;
		if (ld != 0)
			return ld;
		for (i in 0...a1.length) {
			var d = compareArg(a1[i], a2[i]);
			if (d != 0)
				return d;
		}
		return 0;
		#else
		var p1 = k1.getParameters();
		var p2 = k2.getParameters();
		if (p1.length == 0 && p2.length == 0)
			return 0;
		return compareArgs(p1, p2);
		#end
	}

	function compareArgs(a1:Array<Dynamic>, a2:Array<Dynamic>):Int {
		var ld = a1.length - a2.length;
		if (ld != 0)
			return ld;
		for (i in 0...a1.length) {
			var d = compareArg(a1[i], a2[i]);
			if (d != 0)
				return d;
		}
		return 0;
	}

	function getSortIndex(v:{}) {
		if (sortIndices.exists(v)) {
			return sortIndices[(v)];
		}
		var sort = sortIndicesCounter++;
		sortIndices[v] = sort;
		return sort;
	}

	function compareArg(v1:Dynamic, v2:Dynamic):Int {
		var vt1 = Type.typeof(v1);
		var vt2 = Type.typeof(v1);
		return switch [vt1, vt2] {
			case [TNull, TNull]:
				// null is always equal to itself
				0;
			case [TInt, TInt] | [TFloat, TFloat] | [TBool, TBool]:
				// Basic types can be compared directly
				Reflect.compare(v1, v2);
			case [TClass(String), TClass(String)]:
				// Strings as well
				Reflect.compare(v1, v2);
			case [TEnum(_), TEnum(_)]:
				// For enum values we recurse
				compare(v1, v2);
			case [TObject, TObject] | [TClass(_), TClass(_)]:
				// Objects get a sort index associated with them which defines the ordering
				Reflect.compare(getSortIndex(v1), getSortIndex(v2));
			case [TFunction, TFunction] | [TUnknown, TUnknown]:
				// We cannot compare functions and the unknown
				throw 'Unsupported comparison types: $vt1 $vt2';
			case _:
				// If the types differ, we sort by the ValueType index
				Reflect.compare(vt1.getIndex(), vt2.getIndex());
		}
	}

	override function copy():EnumValueMap<K, V> {
		var copied = new EnumValueMap<K, V>();
		copied.root = root;
		return copied;
	}
}
