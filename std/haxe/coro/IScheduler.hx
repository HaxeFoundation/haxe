package haxe.coro;

interface IScheduler {
    function schedule(func:() -> Void):Void;
}