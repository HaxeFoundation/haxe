package ds;

import haxe.exceptions.ArgumentException;
import hxcoro.ds.Out;
import hxcoro.ds.ConcurrentCircularBuffer;

class TestConcurrentCircularBuffer extends utest.Test {
	public function test_invalid_capacity() {
		Assert.raises(() -> new ConcurrentCircularBuffer(0), ArgumentException);
	}

	public function test_push_pop() {
		final buffer = new ConcurrentCircularBuffer(3);

		Assert.isTrue(buffer.tryPush(1));
		Assert.isTrue(buffer.tryPush(2));
		Assert.isTrue(buffer.tryPush(3));
		Assert.isFalse(buffer.tryPush(4));

		final out = new Out();
		Assert.isTrue(buffer.tryPop(out));
		Assert.equals(1, out.get());
		Assert.isTrue(buffer.tryPop(out));
		Assert.equals(2, out.get());
		Assert.isTrue(buffer.tryPop(out));
		Assert.equals(3, out.get());

		Assert.isFalse(buffer.tryPop(out));
	}

	public function test_push_pop_wrap_around() {
		final buffer = new ConcurrentCircularBuffer(3);
		final out    = new Out();

		for (i in 0...10) {
			Assert.isTrue(buffer.tryPush(i));
			Assert.isTrue(buffer.tryPop(out));
			Assert.equals(i, out.get());
		}
	}
}