import hl.I64;
import cpp.uv.Timer;
import cpp.uv.Check;
import sys.thread.Thread;

class CheckSample extends UVSample {
	public function run() {
		var loop = Thread.current().events;
		var timer = Timer.init(loop);
		timer.start(() -> {
			timer.stop();
			timer.close();
		}, I64.ofInt(10), I64.ofInt(10));

		var check = Check.init(loop);
		check.start(() -> {
			print('Check');
			check.stop();
			check.close();
		});
	}
}