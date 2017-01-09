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
 package java.vm;

import java.util.concurrent.atomic.AtomicReference;

/**
	A lock-free queue implementation
**/
@:native('haxe.java.vm.AtomicList')
@:nativeGen class AtomicList<T>
{
	@:volatile @:private var head:AtomicNode<T>;
	@:volatile @:private var tail:AtomicReference<AtomicNode<T>>;

	public function new()
	{
		this.head = new AtomicNode(null);
		this.head.set(new AtomicNode(null));
		this.tail = new AtomicReference(head);
	}

	public function add(v:T)
	{
		var n = new AtomicNode(v), tail = this.tail;
		var p = null;
		while( !((p = tail.get()).compareAndSet(null, n)) )
		{
			tail.compareAndSet(p, p.get());
		}
		tail.compareAndSet(p, n);
	}

	public function pop():Null<T>
	{
		var p = null, pget = null, head = head;
		do
		{
			p = head.get();
			if ( (pget = p.get()) == null)
				return null; //empty
		} while(!head.compareAndSet(p, pget));

		var ret = pget.value;
		pget.value = null;
		return ret;
	}

	public function peek()
	{
		var ret = head.get();
		if (ret == null) return null; //empty
		return ret.value;
	}

	public function peekLast()
	{
		return tail.get().value;
	}

}

@:native('haxe.java.vm.AtomicNode')
@:nativeGen class AtomicNode<T> extends AtomicReference<AtomicNode<T>>
{
	public var value:T;

	public function new(value)
	{
		super();
		this.value = value;
	}

}
