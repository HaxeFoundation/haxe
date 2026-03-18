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
import java.lang.ref.WeakReference;
import java.lang.ref.ReferenceQueue;

private class Registration<T> extends WeakReference<Dynamic> {
	public var heldValue:Null<T>;
	public var callback:Null<T->Void>;
	public var cancelled:AtomicBool;

	public function new(target:Dynamic, heldValue:T, callback:T->Void, queue:ReferenceQueue<Dynamic>) {
		super(target, queue);
		this.heldValue = heldValue;
		this.callback = callback;
		this.cancelled = new AtomicBool(false);
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
	var queue:ReferenceQueue<Dynamic>;
	var allRegs:Array<Registration<T>>;

	public function new(callback:T->Void) {
		this.callback = callback;
		this.queue = new ReferenceQueue();
		this.allRegs = [];
	}

	function pollQueue():Void {
		var ref:Dynamic = null;
		while ((ref = queue.poll()) != null) {
			var reg:Registration<T> = cast ref;
			if (reg.cancelled.compareExchange(false, true) == false) {
				reg.callback(reg.heldValue);
				reg.callback = null;
				reg.heldValue = null;
			}
			allRegs.remove(reg);
		}
	}

	public function register(target:{}, heldValue:T):IHandle {
		pollQueue();
		var reg = new Registration(target, heldValue, callback, queue);
		allRegs.push(reg);

		return new Handle(reg);
	}
}
