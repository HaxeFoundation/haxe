import cpp.uv.Process;
import cpp.uv.Misc;
import cpp.uv.Signal;
import sys.thread.Thread;

class SignalSample extends UVSample {
	public function run() {
		var loop = Thread.current().events;
		var signal = Signal.init(loop);
		print('waiting for Ctrl^C...');
		signal.start(SIGINT, () -> {
			print('got Ctrl^C');
			signal.stop();
			signal.close();
		});
		var selfPid = Misc.getPid();
		Process.killPid(selfPid, SIGINT);
	}
}