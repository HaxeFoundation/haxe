package haxe.coro;

typedef Continuation<Result, Error> = (result:Result, error:Error) -> Void;