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

import haxe.ds.ObjectMap;

private class Registration<T> {
	public var heldValue:T;
	public var cancelled:Bool;
	public var callback:T->Void;

	public function new(heldValue:T, callback:T->Void) {
		this.heldValue = heldValue;
		this.cancelled = false;
		this.callback = callback;
	}
}

@:coreApi
class GcFinalizer<T> {
	var callback:T->Void;
	var tokenMap:ObjectMap<{}, Array<Registration<T>>>;

	public function new(callback:T->Void) {
		this.callback = callback;
		this.tokenMap = new ObjectMap();
	}

	public function register(target:{}, heldValue:T, ?unregisterToken:{}):Void {
		var reg = new Registration(heldValue, callback);

		var regs:Array<Dynamic> = Reflect.field(target, "__hx_gc_regs");
		if (regs == null) {
			regs = [];
			Reflect.setField(target, "__hx_gc_regs", regs);
			cpp.vm.Gc.setFinalizer(target, cpp.Callable.fromStaticFunction(_invoke));
		}
		regs.push(reg);

		if (unregisterToken != null) {
			var list = tokenMap.get(unregisterToken);
			if (list == null) {
				list = [];
				tokenMap.set(unregisterToken, list);
			}
			list.push(reg);
		}
	}

	public function unregister(unregisterToken:{}):Void {
		var list = tokenMap.get(unregisterToken);
		if (list != null) {
			for (reg in list) {
				reg.cancelled = true;
			}
			tokenMap.remove(unregisterToken);
		}
	}

	static function _invoke(obj:Dynamic):Void {
		var regs:Array<Dynamic> = Reflect.field(obj, "__hx_gc_regs");
		if (regs != null) {
			var i = 0;
			while (i < regs.length) {
				var reg:Dynamic = regs[i];
				if (reg.cancelled != true) {
					reg.callback(reg.heldValue);
				}
				i++;
			}
		}
	}
}
