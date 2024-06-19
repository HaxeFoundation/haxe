/*
 * Copyright (C)2005-2024 Haxe Foundation
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

package js.lib;

class HaxeKeyValueIterator<K, V> {
	final jsIterator:js.lib.Iterator<KeyValue<K, V>>;
	var lastStep:js.lib.Iterator.IteratorStep<KeyValue<K, V>>;

	public inline function new(jsIterator:js.lib.Iterator<KeyValue<K, V>>) {
		this.jsIterator = jsIterator;
		lastStep = jsIterator.next();
	}

	public inline function hasNext():Bool {
		return !lastStep.done;
	}

	public inline function next():{key:K, value:V} {
		var v = lastStep.value;
		lastStep = jsIterator.next();
		return {key: v.key, value: v.value};
	}

	public static inline function keyValueIterator<K, V>(jsIterator:js.lib.Iterator<KeyValue<K, V>>) {
		return new HaxeKeyValueIterator(jsIterator);
	}
}
