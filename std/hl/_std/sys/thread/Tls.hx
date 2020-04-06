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

package sys.thread;

#if doc_gen
@:coreApi
extern class Tls<T> {
	var value(get, set):T;
	function new():Void;
}
#else

@:hlNative("std")
abstract Tls<T>(hl.Abstract<"hl_tls">) {
	public var value(get, set):T;

	public function new() {
		this = tls_alloc(true);
	}

	function get_value():T {
		return tls_get(this);
	}

	function set_value(v:T) {
		tls_set(this, v);
		return v;
	}

	static function tls_alloc(gcValue:Bool)
		return null;

	static function tls_get(t):Dynamic
		return null;

	static function tls_set(t, v:Dynamic) {}
}
#end
