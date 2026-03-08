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

package haxe;

import python.lib.Weakref.PythonFinalizer;
import haxe.ds.ObjectMap;

@:coreApi
class GcFinalizer<T> {
	var callback:T->Void;
	var tokenMap:ObjectMap<{}, Array<PythonFinalizer>>;

	public function new(callback:T->Void) {
		this.callback = callback;
		this.tokenMap = new ObjectMap();
	}

	public function register(target:{}, heldValue:T, ?unregisterToken:{}):Void {
		var fin = new PythonFinalizer(target, callback, heldValue);
		if (unregisterToken != null) {
			var list = tokenMap.get(unregisterToken);
			if (list == null) {
				list = [];
				tokenMap.set(unregisterToken, list);
			}
			list.push(fin);
		}
	}

	public function unregister(unregisterToken:{}):Void {
		var list = tokenMap.get(unregisterToken);
		if (list != null) {
			for (fin in list) {
				fin.detach();
			}
			tokenMap.remove(unregisterToken);
		}
	}
}
