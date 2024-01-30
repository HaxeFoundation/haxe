package unit.issues;

private enum abstract Message<TBody>(Int) {
	final move_to:Message<{x:Int, y:Int}>;
}

private typedef Listener = {
	function send<TBody>(name:Message<TBody>, body:TBody):Void;
}

private final listener:Listener = {
	send: function send<T>(message:Message<T>, body:T) {
		switch (message) {
			case move_to:
		}
	}
}

class Issue11513 extends Test {
	function test() {
		listener.send(move_to, {x: 1, y: 2});
		noAssert();
	}
}
