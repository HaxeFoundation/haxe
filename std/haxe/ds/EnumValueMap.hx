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

package haxe.ds;

/**
	EnumValueMap allows mapping of enum value keys to arbitrary values.

	Keys are compared by value and recursively over their parameters. If any
	parameter is not an enum value, `Reflect.compare` is used to compare them.
**/
class EnumValueMap<K:EnumValue, V> extends haxe.ds.BalancedTree<K, V> implements haxe.Constraints.IMap<K,V> {

	override function compare(k1:EnumValue, k2:EnumValue):Int {
		var d = k1.getIndex() - k2.getIndex();
		if (d != 0) return d;
		var p1 = k1.getParameters();
		var p2 = k2.getParameters();
		if (p1.length == 0 && p2.length == 0) return 0;
		return compareArgs(p1, p2);
	}

	function compareArgs(a1:Array<Dynamic>, a2:Array<Dynamic>):Int {
		var ld = a1.length - a2.length;
		if (ld != 0) return ld;
		for (i in 0...a1.length) {
			var d = compareArg(a1[i], a2[i]);
			if (d != 0) return d;
		}
		return 0;
	}

	function compareArg(v1:Dynamic, v2:Dynamic):Int {
		return if (Reflect.isEnumValue(v1) && Reflect.isEnumValue(v2)) {
			compare(v1, v2);
		} else if (Std.is(v1, Array) && Std.is(v2, Array)) {
			compareArgs(v1, v2);
		} else {
			Reflect.compare(v1, v2);
		}
	}
}