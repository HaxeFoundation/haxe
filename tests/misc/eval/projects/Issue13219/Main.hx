import sys.net.Host;
import sys.net.Socket;

// A blocking socket call with a timeout was interrupted (EINTR) shortly after
// a process had been waited for, on Linux.
class Main {
	static function main() {
		if (Sys.systemName() != "Linux")
			return;
		final host = new Host("127.0.0.1");
		final server = new Socket();
		server.bind(host, 0);
		server.listen(1);
		final port = server.host().port;
		// fill the backlog, so that the next connection has to wait
		final pending = [for (_ in 0...2) {
			final s = new Socket();
			s.connect(host, port);
			s;
		}];

		final p = new sys.io.Process("true", []);
		p.exitCode();
		p.close();

		sys.thread.Thread.create(() -> {
			Sys.sleep(0.5);
			for (_ in 0...3)
				server.accept();
		});
		final s = new Socket();
		s.setTimeout(10);
		s.connect(host, port);
	}
}
