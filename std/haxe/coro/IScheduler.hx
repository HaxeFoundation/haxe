package haxe.coro;

interface IScheduler {
    function schedule(func:() -> Void):Void;
    function scheduleIn(func:() -> Void, ms:Int):Void;
}