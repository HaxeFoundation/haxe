package issues.aidan;

import haxe.exceptions.NotImplementedException;

function throwing() {
	throw new NotImplementedException();
}

@:coroutine function recursion(i:Int, acc:Int) {
	yield();
	return if (i > 0) {
		recursion(i - 1, acc + i);
	} else {
		throwing();
	}
}
class Issue59 extends utest.Test {
	public function test() {
		Assert.raises(() -> CoroRun.run(() -> recursion(2, 0)), NotImplementedException);
	}
}