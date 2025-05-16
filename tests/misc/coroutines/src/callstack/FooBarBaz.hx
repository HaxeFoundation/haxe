package callstack;

import haxe.Exception;
import haxe.coro.Coroutine.yield;

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