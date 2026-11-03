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
		Syntax.code("for i,v in {0}({1}) do ({2})(i,v) end", Lua.ipairs, table, func);
	}

	public static function pairsEach<A, B>(table:Table<A, B>, func:A->B->Void):Void {
		Syntax.code("for k,v in {0}({1}) do ({2})(k,v) end", Lua.pairs, table, func);
	}

	public static function ipairsMap<A, B>(table:Table<Dynamic, A>, func:Int->A->B):Table<Int, B> {
		var ret:Table<Int, B> = Table.create();
		Syntax.code("for i,v in {0}({1}) do {2}[i] = ({3})(i,v) end", Lua.ipairs, table, ret, func);
		return ret;
	}

	public static function pairsMap<A, B, C>(table:Table<A, B>, func:A->B->C):Table<A, C> {
		var ret:Table<A, C> = Table.create();
		Syntax.code("for k,v in {0}({1}) do {2}[k] = ({3})(k,v) end", Lua.pairs, table, ret, func);
		return ret;
	}

	public static function ipairsFold<A, B>(table:Table<Int, A>, func:Int->A->B->B, seed:B):B {
		return Syntax.code("(function() local s = {0}; for i,v in {1}({2}) do s = ({3})(i,v,s) end; return s end)()", seed, Lua.ipairs, table, func);
	}

	public static function pairsFold<A, B, C>(table:Table<A, B>, func:A->B->C->C, seed:C):C {
		return Syntax.code("(function() local s = {0}; for k,v in {1}({2}) do s = ({3})(k,v,s) end; return s end)()", seed, Lua.pairs, table, func);
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
		Syntax.code("for k,v in {0}({1}) do if ({2})(k,v) then return true end end", Lua.ipairs, table, func);
	}

	public static function pairsExist<A, B>(table:Table<A, B>, func:A->B->Bool) {
		Syntax.code("for k,v in {0}({1}) do if ({2})(k,v) then return true end end", Lua.pairs, table, func);
	}

	public static function copy<A, B>(table1:Table<A, B>):Table<A, B> {
		var ret:Table<A, B> = Table.create();
		Syntax.code("for k,v in {0}({1}) do {2}[k] = v end", Lua.pairs, table1, ret);
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
