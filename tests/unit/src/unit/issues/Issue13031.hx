package unit.issues;

class Issue13031 extends Test {
	#if sys
	function testHostHeader() {
		for (test in [
			{url: "http://127.0.0.1/status", host: "127.0.0.1", port: 80},
			{url: "http://127.0.0.1:4444/status", host: "127.0.0.1:4444", port: 4444},
			{url: "127.0.0.1:4444/status", host: "127.0.0.1:4444", port: 4444},
			{url: "http://127.0.0.1:80/status", host: "127.0.0.1:80", port: 80},
			{url: "https://127.0.0.1/status", host: "127.0.0.1", port: 443},
			{url: "https://127.0.0.1:443/status", host: "127.0.0.1:443", port: 443},
			{url: "https://127.0.0.1:8443/status", host: "127.0.0.1:8443", port: 8443}
		]) {
			var socket = new RequestSocket();
			var request = new haxe.Http(test.url);
			request.onError = message -> assert(message);
			request.customRequest(false, new haxe.io.BytesOutput(), socket);
			socket.close();
			eq('GET /status HTTP/1.1\r\nHost: ${test.host}\r\nConnection: close\r\n\r\n', socket.request.getBytes().toString());
			eq(test.port, socket.connectedPort);
		}
	}
	#end
}

#if sys
private class RequestSocket extends sys.net.Socket {
	public final request = new haxe.io.BytesOutput();
	public var connectedPort:Int;
	final originalInput:haxe.io.Input;
	final originalOutput:haxe.io.Output;

	public function new() {
		super();
		originalInput = input;
		originalOutput = output;
		input = new haxe.io.BytesInput(haxe.io.Bytes.ofString("HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n"));
		output = request;
	}

	public override function connect(host:sys.net.Host, port:Int) {
		connectedPort = port;
	}

	public override function close() {
		input = originalInput;
		output = originalOutput;
		#if !lua
		super.close();
		#end
	}
}
#end
