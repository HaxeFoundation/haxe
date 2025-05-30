package issues.aidan;

import haxe.exceptions.NotImplementedException;

function throwing(v:Dynamic) {
	throw v;
}

@:coroutine function foo(v:Dynamic) {
	var s = try {
		throwing(v);
		"";
	} catch (s:String) {
		s;
	}
	return s;
}

class Issue55 extends utest.Test {
	public function test() {
		Assert.equals("caught", CoroRun.run(() -> foo("caught")));
		Assert.raises(() -> CoroRun.run(() -> foo(new haxe.exceptions.NotImplementedException())), NotImplementedException);
	}
}