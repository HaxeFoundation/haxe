package unit.issues;

class Issue8401 extends unit.Test {
#if python
	function testNew() {
		var sock = new python.net.SslSocket();
		// With Issue8401, construction fails immediately; if we get this far, it's a pass
		utest.Assert.pass();
	}

	@:access(python.net.SslSocket.__s)
	@:access(python.net.SslSocket.wrapSocketWithSslContext)
	function testTimeout() {
		var sock = new python.net.SslSocket();
		// gettimeout is not currently defined in the python socket extern, but it's
		// present in the python socket type.
		eq(null, (cast sock.__s).gettimeout());
		sock.setTimeout(500);
		eq(500, (cast sock.__s).gettimeout());
		// This will change __s.  Make sure we set the timeout properly.
		sock.wrapSocketWithSslContext("127.0.0.1");
		eq(500, (cast sock.__s).gettimeout());
	}

	#if (python_verion >= 3.7)
	@:access(python.net.SslSocket.__s)
	@:access(python.net.SslSocket.wrapSocketWithSslContext)
	function testBlocking() {
		var sock = new python.net.SslSocket();
		// getblocking is not currently defined in the python socket extern, but it's
		// present in the python socket type.
		t((cast sock.__s).getblocking());
		sock.setBlocking(false);
		f((cast sock.__s).getblocking());
		// This will change __s.  Make sure we set the timeout properly.
		sock.wrapSocketWithSslContext("127.0.0.1");
		f((cast sock.__s).getblocking());
	}
	#end

	@:access(python.net.SslSocket.__s)
	@:access(python.net.SslSocket.wrapSocketWithSslContext)
	function testFastSend() {
		var sock = new python.net.SslSocket();
		// getsockopt is not currently defined in the python socket extern, but it's
		// present in the python socket type.
		eq(0, (cast sock.__s).getsockopt(python.lib.Socket.SOL_TCP, python.lib.Socket.TCP_NODELAY));
		sock.setFastSend(true);
		eq(1, (cast sock.__s).getsockopt(python.lib.Socket.SOL_TCP, python.lib.Socket.TCP_NODELAY));
		// This will change __s.  Make sure we set the timeout properly.
		sock.wrapSocketWithSslContext("127.0.0.1");
		eq(1, (cast sock.__s).getsockopt(python.lib.Socket.SOL_TCP, python.lib.Socket.TCP_NODELAY));
	}
#end
}
