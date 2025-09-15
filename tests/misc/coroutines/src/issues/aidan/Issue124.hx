package issues.aidan;

import haxe.coro.Coroutine;
import haxe.coro.context.Context;
import hxcoro.task.ICoroTask;
import hxcoro.task.CoroTask;
import hxcoro.ds.channels.Channel;
import hxcoro.task.ICoroNode;

using issues.aidan.Issue124.NumberProducer;

interface IReceiver<T> extends ICoroTask<haxe.Unit> {
	@:coroutine function receive():T;
}

interface ISender<T> {
	@:coroutine function send(v:T):Void;
}

class CoroChannelTask<T> extends CoroTask<haxe.Unit> implements IReceiver<T> implements ISender<T> {
	final channel:Channel<T>;

	public function new(context:Context, channel:Channel<T>) {
		super(context, CoroTask.CoroScopeStrategy);
		this.channel = channel;
	}

	@:coroutine public function receive() {
		return channel.reader.read();
	}

	@:coroutine public function send(v:T) {
		return channel.writer.write(v);
	}
}

function produce<T>(context:Context, lambda:Coroutine<ISender<T>->Void>):IReceiver<T> {
	final channel = Channel.createBounded({ size : 3 });
	final task = new CoroChannelTask(context, channel);
	final result = lambda(task, task);
	switch result.state {
		case Pending:

		case Returned:
			task.resume(result.result, null);
		case Thrown:
			task.resume(null, result.error);
	}
	return task;
}

class NumberProducer {
	static public function produceNumbers(node:ICoroNode) {
		return produce(node.context, node -> {
			var i = 1;
			while (true) {
				node.send(i++);
			}
		});
	}

	static public function square(node:ICoroNode, numbers:IReceiver<Int>) {
		return produce(node.context, node -> {
			while (true) {
				var x = numbers.receive();
				node.send(x * x);
			}
		});
	}

	static public function numbersFrom(node:ICoroNode, start:Int) {
		return produce(node.context, node -> {
			var i = start;
			while (true) {
				node.send(i++);
			}
		});
	}

	static public function filter(node:ICoroNode, numbers:IReceiver<Int>, prime:Int) {
		return produce(node.context, node -> {
			while (true) {
				final x = numbers.receive();
				if (x % prime != 0) {
					node.send(x);
				}
			}
		});
	}
}

class Issue124 extends utest.Test {
	function test() {
		final result = CoroRun.runScoped(node -> {
			final numbers = node.produceNumbers();
			final squares = node.square(numbers);
			final result = [for (i in 1...10) {
				squares.receive();
			}];
			node.cancelChildren();
			result;
		});
		Assert.same([1, 4, 9, 16, 25, 36, 49, 64, 81], result);
	}

	function testPrime() {
		final result = CoroRun.runScoped(node -> {
			var cur = node.numbersFrom(2);
			final result = [
				for (_ in 0...10) {
					final prime = cur.receive();
					cur = node.filter(cur, prime);
					prime;
				}
			];
			node.cancelChildren();
			result;
		});
		Assert.same([2, 3, 5, 7, 11, 13, 17, 19, 23, 29], result);
	}
}