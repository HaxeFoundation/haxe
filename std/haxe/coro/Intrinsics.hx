package haxe.coro;

extern class Intrinsics {
    public static function currentContinuation():IContinuation<Any>;
}