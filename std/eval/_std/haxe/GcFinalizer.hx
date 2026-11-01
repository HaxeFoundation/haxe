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

import haxe.atomic.AtomicBool;

private class Registration<T> {
	public var heldValue:Null<T>;
	public var cancelled:AtomicBool;
	public var callback:Null<T->Void>;

	public function new(heldValue:T, callback:T->Void) {
		this.heldValue = heldValue;
		this.cancelled = new AtomicBool(false);
		this.callback = callback;
	}
}

private class Handle<T> implements IHandle {
	var reg:Registration<T>;

	public function new(reg:Registration<T>) {
		this.reg = reg;
	}

	public function close():Void {
		if (reg.cancelled.compareExchange(false, true) == false) {
			reg.callback = null;
			reg.heldValue = null;
		}
	}
}

@:coreApi
class GcFinalizer<T> {
	var callback:T->Void;

	public function new(callback:T->Void) {
		this.callback = callback;
	}

	public function register(target:{}, heldValue:T):IHandle {
		var reg = new Registration(heldValue, callback);
		eval.vm.Gc.finalise(function(_) {
			if (reg.cancelled.compareExchange(false, true) == false) {
				reg.callback(reg.heldValue);
				reg.callback = null;
				reg.heldValue = null;
			}
		}, target);

		return new Handle(reg);
	}
}
