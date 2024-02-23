package haxe.coro;

typedef Continuation<Result> = (result:Result, control:CoroutineControl) -> Void;
