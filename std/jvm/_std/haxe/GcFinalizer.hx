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

import java.lang.ref.WeakReference;
import java.lang.ref.ReferenceQueue;
import haxe.ds.ObjectMap;

private class Registration<T> extends WeakReference<Dynamic> {
	public var heldValue:T;
	public var callback:T->Void;
	public var cancelled:Bool;

	public function new(target:Dynamic, heldValue:T, callback:T->Void, queue:ReferenceQueue<Dynamic>) {
		super(target, queue);
		this.heldValue = heldValue;
		this.callback = callback;
		this.cancelled = false;
	}
}

@:coreApi
class GcFinalizer<T> {
	var callback:T->Void;
	var queue:ReferenceQueue<Dynamic>;
	var tokenMap:ObjectMap<{}, Array<Registration<T>>>;
	var allRegs:Array<Registration<T>>;

	public function new(callback:T->Void) {
		this.callback = callback;
		this.queue = new ReferenceQueue();
		this.tokenMap = new ObjectMap();
		this.allRegs = [];
	}

	function pollQueue():Void {
		var ref:Dynamic = null;
		while ((ref = queue.poll()) != null) {
			var reg:Registration<T> = cast ref;
			if (!reg.cancelled) {
				reg.callback(reg.heldValue);
			}
			reg.heldValue = null;
			reg.callback = null;
			allRegs.remove(reg);
		}
	}

	public function register(target:{}, heldValue:T, ?unregisterToken:{}):Void {
		pollQueue();
		var reg = new Registration(target, heldValue, callback, queue);
		allRegs.push(reg);

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
		pollQueue();
		var list = tokenMap.get(unregisterToken);
		if (list != null) {
			for (reg in list) {
				reg.cancelled = true;
			}
			tokenMap.remove(unregisterToken);
		}
	}
}
