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

package lua;

/**
	A set of utility methods for working with the Lua table extern.
**/
class PairTools {
	public static function ipairsEach<T>(table:Table<Dynamic, T>, func:Int->T->Void):Void {
		untyped __lua__("for i,v in _G.ipairs(table) do func(i,v) end");
	}

	public static function pairsEach<A, B>(table:Table<A, B>, func:A->B->Void):Void {
		untyped __lua__("for k,v in _G.pairs(table) do func(k,v) end");
	}

	public static function ipairsMap<A, B>(table:Table<Dynamic, A>, func:Int->A->B):Table<Int, B> {
		var ret:Table<Int, B> = Table.create();
		untyped __lua__("for i,v in _G.ipairs(table) do ret[i] = func(i,v) end");
		return ret;
	}

	public static function pairsMap<A, B, C>(table:Table<A, B>, func:A->B->C->C):Table<A, C> {
		var ret:Table<A, C> = Table.create();
		untyped __lua__("for k,v in _G.pairs(table) do ret[k] = func(k,v) end");
		return ret;
	}

	public static function ipairsFold<A, B>(table:Table<Int, A>, func:Int->A->B->B, seed:B):B {
		untyped __lua__("for i,v in _G.ipairs(table) do seed = func(i,v,seed) end");
		return untyped __lua__("seed");
	}

	public static function pairsFold<A, B, C>(table:Table<A, B>, func:A->B->C->C, seed:C):C {
		untyped __lua__("for k,v in _G.pairs(table) do seed = func(k,v,seed) end");
		return untyped __lua__("seed");
	}

	public static function ipairsConcat<T>(table1:Table<Int, T>, table2:Table<Int, T>) {
		var ret:Table<Int, T> = Table.create();
		ipairsFold(table1, function(a, b, c:Table<Int, T>) {
			c[a] = b;
			return c;
		}, ret);
		var size = lua.TableTools.maxn(ret);
		ipairsFold(table2, function(a, b, c:Table<Int, T>) {
			c[a + size] = b;
			return c;
		}, ret);
		return ret;
	}

	public static function pairsMerge<A, B>(table1:Table<A, B>, table2:Table<A, B>) {
		var ret = copy(table1);
		pairsEach(table2, function(a, b:B) ret[cast a] = b);
		return ret;
	}

	public static function ipairsExist<T>(table:Table<Int, T>, func:Int->T->Bool) {
		untyped __lua__("for k,v in _G.ipairs(table) do if func(k,v) then return true end end");
	}

	public static function pairsExist<A, B>(table:Table<A, B>, func:A->B->Bool) {
		untyped __lua__("for k,v in _G.pairs(table) do if func(k,v) then return true end end");
	}

	public static function copy<A, B>(table1:Table<A, B>):Table<A, B> {
		var ret:Table<A, B> = Table.create();
		untyped __lua__("for k,v in _G.pairs(table1) do ret[k] = v end");
		return ret;
	}

	public static function pairsIterator<A, B>(table:Table<A, B>):Iterator<{index:A, value:B}> {
		var p = Lua.pairs(table);
		var next = p.next;
		var i = p.index;
		return {
			next: function() {
				var res = next(table, i);
				i = res.index;
				return {index: res.index, value: res.value};
			},
			hasNext: function() {
				return Lua.next(table, i).value != null;
			}
		}
	}

	public static function ipairsIterator<A, B>(table:Table<A, B>):Iterator<{index:Int, value:B}> {
		var p = Lua.ipairs(table);
		var next = p.next;
		var i = p.index;
		return {
			next: function() {
				var res = next(table, i);
				i = res.index;
				return {index: res.index, value: res.value};
			},
			hasNext: function() {
				return next(table, i).value != null;
			}
		}
	}
}
