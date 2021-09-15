import cpp.uv.Process;
import sys.thread.Thread;

class ProcessSample extends UVSample {
	public function run() {
		// Process.disableStdioInheritance();

		var env = Sys.environment();
		env.set('DUMMY', '123');

		var cmd = switch Sys.systemName() {
			case 'Windows': 'dir';
			case _: 'ls';
		}

		var opt:ProcessOptions = {
			cwd: '..',
			stdio: [INHERIT, INHERIT, INHERIT],
			env: env,
			onExit: (p, exitCode, termSignal) -> {
				print('process finished: exitCode $exitCode, termSignal $termSignal');
				p.close(() -> print('process closed'));
			},
		}
		var args = [cmd];
		// args.push('non-existent-path');
		var p = Process.spawn(Thread.current().events, cmd, args, opt);
		print('pid ${p.pid}');
		// p.kill(SIGINT);
		// Process.killPid(p.pid, SIGINT);
	}
}