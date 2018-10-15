package unit.issues;

class Issue4581 extends unit.Test {
    #if php
	function test() {
		var host = new sys.net.Host('google.com');
        var socket = new php.net.SslSocket();
        socket.connect(host, 443);
	}
    #end
}