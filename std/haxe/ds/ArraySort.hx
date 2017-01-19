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
	ArraySort provides a stable implementation of merge sort through its `sort`
	method. It should be used instead of `Array.sort` in cases where the order
	of equal elements has to be retained on all targets.
**/
class ArraySort {

	/**
		Sorts Array `a` according to the comparison function `cmp`, where
		`cmp(x,y)` returns 0 if `x == y`, a positive Int if `x > y` and a
		negative Int if `x < y`.

		This operation modifies Array `a` in place.

		This operation is stable: The order of equal elements is preserved.

		If `a` or `cmp` are null, the result is unspecified.
	**/
	static public function sort<T>(a:Array<T>, cmp:T -> T -> Int) {
		rec(a, cmp, 0, a.length);
	}

	static function rec<T>(a:Array<T>, cmp, from, to) {
		var middle = (from + to) >> 1;
		if (to - from < 12) {
			if (to <= from) return;
			for (i in (from + 1)...to) {
				var j = i;
				while (j > from) {
					if (compare(a, cmp, j, j - 1) < 0)
						swap(a, j - 1, j);
					else
						break;
					j--;
				}
			}
			return;
		}
		rec(a, cmp, from, middle);
		rec(a, cmp, middle, to);
		doMerge(a, cmp, from, middle, to, middle - from, to - middle);
	}

	static function doMerge<T>(a:Array<T>, cmp, from, pivot, to, len1, len2) {
		var first_cut, second_cut, len11, len22, new_mid;
		if (len1 == 0 || len2 == 0)
			return;
		if (len1 + len2 == 2) {
			if (compare(a, cmp, pivot, from) < 0)
				swap(a, pivot, from);
			return;
		}
		if (len1 > len2) {
			len11 = len1 >> 1;
			first_cut = from + len11;
			second_cut = lower(a, cmp, pivot, to, first_cut);
			len22 = second_cut - pivot;
		} else {
			len22 = len2 >> 1;
			second_cut = pivot + len22;
			first_cut = upper(a, cmp, from, pivot, second_cut);
			len11 = first_cut - from;
		}
		rotate(a, cmp, first_cut, pivot, second_cut);
		new_mid = first_cut + len22;
		doMerge(a, cmp, from, first_cut, new_mid, len11, len22);
		doMerge(a, cmp, new_mid, second_cut, to, len1 - len11, len2 - len22);
	}

	static function rotate<T>(a:Array<T>, cmp, from, mid, to) {
		var n;
		if (from == mid || mid == to) return;
		n = gcd(to - from, mid - from);
		while (n-- != 0) {
			var val = a[from + n];
			var shift = mid - from;
			var p1 = from + n, p2 = from + n + shift;
			while (p2 != from + n) {
				a[p1] = a[p2];
				p1 = p2;
				if (to - p2 > shift) p2 += shift;
				else p2 = from + (shift - (to - p2));
			}
			a[p1] = val;
		}
	}

	static function gcd(m, n) {
		while (n != 0) {
			var t = m % n;
			m = n;
			n = t;
		}
		return m;
	}

	static function upper<T>(a:Array<T>, cmp, from, to, val) {
		var len = to - from, half, mid;
		while (len > 0) {
			half = len >> 1;
			mid = from + half;
			if (compare(a, cmp, val, mid) < 0)
				len = half;
			else {
				from = mid + 1;
				len = len - half - 1;
			}
		}
		return from;
	}

	static function lower<T>(a:Array<T>, cmp, from, to, val) {
		var len = to - from, half, mid;
		while (len > 0) {
			half = len >> 1;
			mid = from + half;
			if (compare(a, cmp, mid, val) < 0) {
				from = mid + 1;
				len = len - half - 1;
			} else
				len = half;
		}
		return from;
	}

	static function swap<T>(a:Array<T>, i, j) {
		var tmp = a[i];
		a[i] = a[j];
		a[j] = tmp;
	}

	static inline function compare<T>(a:Array<T>, cmp, i, j) {
		return cmp(a[i], a[j]);
	}
}