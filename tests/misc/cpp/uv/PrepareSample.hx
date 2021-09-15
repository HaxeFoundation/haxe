import cpp.uv.Timer;
import cpp.uv.Prepare;
import sys.thread.Thread;

class PrepareSample extends UVSample {
	public function run() {
		var loop = Thread.current().events;
		var timer = Timer.init(loop);
		timer.start(() -> {
			timer.stop();
			timer.close();
		}, 10, 10);

		var prepare = Prepare.init(loop);
		prepare.start(() -> {
			print('Prepare');
			prepare.stop();
			prepare.close();
		});
	}
}