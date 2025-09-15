import haxe.Timer;
import haxe.Int64;
import haxe.Exception;
import haxe.coro.schedulers.Scheduler;
import haxe.coro.schedulers.IScheduleObject;

class ImmediateScheduler extends Scheduler {
	public function new() {
		super();
	}

	public function schedule(ms:Int64, f:() -> Void) {
		if (ms != 0) {
			throw new Exception('Only immediate scheduling is allowed in this scheduler');
		}
		f();
		return null;
	}

	public function scheduleObject(obj:IScheduleObject) {
		obj.onSchedule();
	}

	public function now() {
		return Timer.milliseconds();
	}
}