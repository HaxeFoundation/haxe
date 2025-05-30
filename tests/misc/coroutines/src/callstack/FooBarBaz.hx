package callstack;

import haxe.Exception;
import hxcoro.Coro.*;

@:coroutine function baz() {
	throw new Exception('hello');
}

@:coroutine function bar() {
	yield();
	baz();
}

@:coroutine function foo() {
	bar();
}