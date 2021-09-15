import cpp.uv.Timer;
import cpp.uv.Idle;
import sys.thread.Thread;

class IdleSample extends UVSample {
	public function run() {
		var loop = Thread.current().events;
		var timer = Timer.init(loop);
		timer.start(() -> {
			timer.stop();
			timer.close();
		}, 10, 10);

		var idle = Idle.init(loop);
		idle.start(() -> {
			print('Idle');
			idle.stop();
			idle.close();
		});
	}
}