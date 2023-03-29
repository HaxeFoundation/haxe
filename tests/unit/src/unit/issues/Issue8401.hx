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
		eq(null, sock.__s.gettimeout());
		sock.setTimeout(500);
		eq(500, sock.__s.gettimeout());
		// This will change __s.  Make sure we set the timeout properly.
		sock.wrapSocketWithSslContext("127.0.0.1");
		eq(500, sock.__s.gettimeout());
	}

	#if (python_verion >= 3.7)
	@:access(python.net.SslSocket.__s)
	@:access(python.net.SslSocket.wrapSocketWithSslContext)
	function testBlocking() {
		var sock = new python.net.SslSocket();
		t(sock.__s.getblocking());
		sock.setBlocking(false);
		f(sock.__s.getblocking());
		// This will change __s.  Make sure we set the blocking flag properly.
		sock.wrapSocketWithSslContext("127.0.0.1");
		f(sock.__s.getblocking());
	}
	#end

	@:access(python.net.SslSocket.__s)
	@:access(python.net.SslSocket.wrapSocketWithSslContext)
	function testFastSend() {
		var sock = new python.net.SslSocket();
		eq(0, sock.__s.getsockopt(python.lib.Socket.SOL_TCP, python.lib.Socket.TCP_NODELAY));
		sock.setFastSend(true);
        // NOTE: this number can vary per platform; non-zero means true/enabled
		utest.Assert.notEquals(0, sock.__s.getsockopt(python.lib.Socket.SOL_TCP, python.lib.Socket.TCP_NODELAY));
		// This will change __s.  Make sure we set the sock opt properly.
		sock.wrapSocketWithSslContext("127.0.0.1");
		utest.Assert.notEquals(0, sock.__s.getsockopt(python.lib.Socket.SOL_TCP, python.lib.Socket.TCP_NODELAY));
	}
#end
}
