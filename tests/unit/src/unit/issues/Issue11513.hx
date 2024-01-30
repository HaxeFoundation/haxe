package unit.issues;

#if ((!java || jvm) && !cs)
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

private final listener2:Listener = {
	send: function<T>(message:Message<T>, body:T) {
		switch (message) {
			case move_to:
		}
	}
}
#end

class Issue11513 extends Test {
	function test() {
		#if ((!java || jvm) && !cs)
		listener.send(move_to, {x: 1, y: 2});
		listener2.send(move_to, {x: 1, y: 2});
		#end
		noAssert();
	}
}
