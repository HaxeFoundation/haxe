package hxcoro.ds;

import haxe.ds.Vector;
import haxe.exceptions.ArgumentException;

final class CircularBuffer<T> {
	final storage : Vector<T>;

	var head : Int;

	var tail : Int;

	public function new(capacity : Int) {
		if (capacity < 1) {
			throw new ArgumentException("capacity", "Capacity must be greater than zero");
		}

		// We need +1 since we do a "full" check by comparing the head and tail.
		storage = new Vector(capacity + 1);
		head    = 0;
		tail    = 0;
	}

	public function tryPush(v:T) {
		final nextHead = increment(head);

		return if (tail != nextHead) {
			storage[head] = v;
			head = nextHead;

			true;
		} else {
			false;
		}
	}

	public function tryPeekHead(out:Out<T>) {
		if (wasEmpty()) {
			return false;
		}

		out.set(storage[decrement(head)]);

		return true;
	}

	public function tryPopHead(out:Out<T>) {
		if (wasEmpty()) {
			return false;
		}

		head = decrement(head);

		out.set(storage[head]);

		return true;
	}

	public function tryPopTail(out:Out<T>) {
		if (wasEmpty()) {
			return false;
		}

		out.set(storage[tail]);

		tail = increment(tail);

		return true;
	}

	public function wasEmpty() {
		return head == tail;
	}

	public function wasFull() {
		final nextHead = increment(head);

		return nextHead == tail;
	}

	inline function increment(v : Int) {
		return (v + 1) % storage.length;
	}

	inline function decrement(v : Int) {
		return if (v == 0) {
			storage.length - 1;
		} else {
			v - 1;
		}
	}
}