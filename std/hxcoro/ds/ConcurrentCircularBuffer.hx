package hxcoro.ds;

import haxe.ds.Vector;
import haxe.exceptions.ArgumentException;
import hxcoro.concurrent.AtomicInt;

/**
 * Thread safe FIFO circular buffer.
 * 
 * This buffer supports at most a single producer and a single consumer at any one time,
 * the behaviour when multiple produces and consumers act on the buffer is undefined.
 */
final class ConcurrentCircularBuffer<T> {
	final storage : Vector<T>;

	final head : AtomicInt;

	final tail : AtomicInt;

	public function new(capacity : Int) {
		if (capacity < 1) {
			throw new ArgumentException("capacity", "Capacity must be greater than zero");
		}

		// We need +1 since we do a "full" check by comparing the head and tail.
		storage = new Vector(capacity + 1);
		head    = new AtomicInt(0);
		tail    = new AtomicInt(0);
	}

	/**
	 * Attempts to add an item to the end of the buffer.
	 * @param v Item to add.
	 * @returns `true` if the item was added to the buffer, otherwise `false`.
	 */
	public function tryPush(v : T) {
		final currentTail = tail.load();
		final nextTail    = increment(currentTail);

		if (nextTail != head.load()) {
			storage[currentTail] = v;
			tail.store(nextTail);
			return true;
		}

		return false;
	}

	/**
	 * Attempts to remove an item from the beginning of the buffer.
	 * @param out If this function returns `true` the removed item will be stored in this out object.
	 * @returns `true` if an item was removed from the buffer, otherwise `false`.
	 */
	public function tryPop(out : Out<T>) {
		final currentHead = head.load();
		if (currentHead == tail.load()) {
			return false;
		}

		// Note : We should probably wipe the previous value here to prevent references being kept to otherwise dead objects.
		// is it safe to do a `= null` even if the circular buffer is storing, say, ints?
		out.set(storage[currentHead]);

		head.store(increment(currentHead));

		return true;
	}

	public function wasEmpty() {
		return head.load() == tail.load();
	}

	public function wasFull() {
		final nextTail = increment(tail.load());

		return nextTail == head.load();
	}

	inline function increment(v : Int) {
		return (v + 1) % storage.length;
	}
}