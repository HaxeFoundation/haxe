package haxe.coro;

interface IScheduler {
    function scheduler(func:() -> Void):Void;
}