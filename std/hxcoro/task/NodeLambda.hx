package hxcoro.task;

import haxe.coro.Coroutine;

typedef NodeLambda<T> = Coroutine<(node:ICoroNode) -> T>;
