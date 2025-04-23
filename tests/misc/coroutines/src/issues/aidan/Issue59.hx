package issues.aidan;

import haxe.coro.Coroutine;
import haxe.coro.Coroutine.yield;
import haxe.exceptions.NotImplementedException;

function throwing() {
	throw new NotImplementedException();
}

@:coroutine @:coroutine.debug function recursion(i:Int, acc:Int) {
	yield();
	return if (i > 0) {
		recursion(i - 1, acc + i);
	} else {
		throwing();
	}
}
class Issue59 extends utest.Test {
	public function test() {
		Assert.raises(() -> Coroutine.run(() -> recursion(2, 0)), NotImplementedException);
	}
}