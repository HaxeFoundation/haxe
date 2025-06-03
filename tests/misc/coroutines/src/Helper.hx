import haxe.coro.schedulers.VirtualTimeScheduler;

@:coroutine
function mapCalls<TArg,TRet>(args:Array<TArg>, f:Coroutine<TArg->TRet>):Array<TRet> {
	return [for (arg in args) f(arg)];
}

class SchedulerTools {
	static public function nowMs(scheduler:VirtualTimeScheduler) {
		return Std.int(scheduler.now() * 1000);
	}
}