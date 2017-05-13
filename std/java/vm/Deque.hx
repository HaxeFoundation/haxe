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
import java.Lib;

/**
	A Lock-free Queue implementation
**/
@:native('haxe.java.vm.Deque')
@:nativeGen class Deque<T>
{
	@:private var head:Node<T>;
	@:private var tail:Node<T>;

	public function new()
	{
		this.head = this.tail = new Node(null);
	}

	public function add(i : T)
	{
		var n = new Node(i);
		untyped __lock__(this,
		{
			tail.next = n;
			tail = n;
			try { untyped this.notify(); } catch(e:Dynamic) { throw e; }
		});
	}

	public function push(i : T)
	{
		var n = new Node(i);
		untyped __lock__(this,
		{
			n.next = head.next;
			head.next = n;
			try { untyped this.notify(); } catch(e:Dynamic) { throw e; }
		});
	}

	public function pop(block : Bool) : Null<T>
	{
		var ret = null;
		untyped __lock__(this, {
			var n = null;
			do {
				n = head.next;
				if (n != null)
				{
					ret = n.value;
					n.value = null;
					head = n;
				} else if (block) {
					//block
					try { untyped this.wait(); } catch(e:Dynamic) { throw e; }
				}
			} while( block && n == null );
		});
		return ret;
	}
}

@:native('haxe.java.vm.DequeNode')
@:nativeGen
class Node<T>
{
	public var value:T;
	public var next:Node<T>;

	public function new(val)
	{
		this.value = val;
	}
}
