import sys.FileSystem;
import haxe.Timer;
import sys.io.File;
import cpp.uv.Misc;
import cpp.uv.FsPoll;
import cpp.uv.UVException;
import sys.thread.Thread;

class FsPollSample extends UVSample {
	public function run() {
		var loop = Thread.current().events;
		var poll = FsPoll.init(loop);
		var path = Misc.tmpDir() + '/test-file';
		File.saveContent(path, 'Hello, world');
		poll.start(path, 100, (e, previous, current) -> switch e {
			case UV_NOERR:
				print('FS Poll at $path:');
				print('\tprev: ${previous.toString()}');
				print('\tcurr: ${current.toString()}');
				FileSystem.deleteFile(path);
				poll.stop();
				poll.close();
			case _:
				throw new UVException(e);
		});
		Timer.delay(File.saveContent.bind(path, 'Bye, world'), 50);
	}
}