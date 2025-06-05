package hxcoro.task;

import haxe.coro.Coroutine;

typedef NodeLambda<T, C = Any> = Coroutine<(node:ICoroNode<C>) -> T>;
