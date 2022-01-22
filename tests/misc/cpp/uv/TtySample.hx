import cpp.uv.File;
import cpp.uv.Tty;
import cpp.uv.UVException;
import sys.thread.Thread;

class TtySample extends UVSample {
	public function run() {
		print('opening tty...');
		var tty = Tty.init(Thread.current().events, File.stdout);
		print('setting mode...');
		tty.setMode(MODE_NORMAL);
		print('window size: ' + tty.getWinSize());
		Tty.resetMode();
		//VTermState works on windows only
		// var state = Tty.getVTermState();
		// print('vterm state: ' + state);
		// print('changing vterm state...');
		// Tty.setVTermState(state == TTY_SUPPORTED ? TTY_UNSUPPORTED : TTY_SUPPORTED);
		// print('vterm state: ' + Tty.getVTermState());
		tty.close(() -> print('Done'));
	}
}