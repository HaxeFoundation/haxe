import sys.thread.EventLoop;
import cpp.uv.Process;
import sys.thread.Thread;

class ProcessSample extends UVSample {
	public function run() {
		// Process.disableStdioInheritance();

		var loop = Thread.current().events;

		var cmd = switch Sys.systemName() {
			case 'Windows': 'dir';
			case _: 'ls';
		}
		var args = [cmd];
		// args.push('non-existent-path');

		var options = createOptions();
		options.env = Sys.environment();
		options.env.set('DUMMY', '123');
		var onExit = options.onExit;
		options.onExit = (p, c, s) -> {
			onExit(p, c, s);
			testSignal(loop, cmd, args);
		}

		print('Running: $cmd');
		var p = Process.spawn(Thread.current().events, cmd, args, options);
		print('pid ${p.pid}');
	}

	function testSignal(loop:EventLoop, cmd:String, args:Array<String>) {
		print('Running: $cmd');
		var p = Process.spawn(loop, cmd, args, createOptions());
		print('pid ${p.pid}');
		p.kill(SIGINT);
		Process.killPid(p.pid, SIGINT);
	}

	function createOptions():ProcessOptions {
		return {
			cwd: '..',
			stdio: [INHERIT, INHERIT, INHERIT],
			onExit: (p, exitCode, termSignal) -> {
				print('process finished: exitCode $exitCode, termSignal $termSignal');
				p.close(() -> print('process ${p.pid} closed'));
			},
		}
	}
}