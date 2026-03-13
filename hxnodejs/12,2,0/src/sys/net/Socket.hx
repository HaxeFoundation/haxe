package sys.net;

class SocketInput extends haxe.io.Input {
	var s:Socket;

	public function new(s:Socket) {
		this.s = s;
	}

	override function readByte():Int {
		s.waitData();
		var buf = s.inputData[0];
		var b = buf[s.inputPos++];
		if (s.inputPos == buf.length) {
			s.inputPos = 0;
			s.inputData.shift();
		}
		return b;
	}

	override function readBytes(buf:haxe.io.Bytes, pos:Int, len:Int):Int {
		s.waitData();
		var nbuf = js.node.Buffer.hxFromBytes(buf);
		var startPos = pos;
		while (len > 0) {
			var buf = s.inputData[0];
			if (buf == null)
				break;
			var avail = buf.length - s.inputPos;
			if (avail > len) {
				buf.copy(nbuf, pos, s.inputPos, s.inputPos + len);
				pos += len;
				s.inputPos += len;
				break;
			}
			buf.copy(nbuf, pos, s.inputPos, s.inputPos + avail);
			pos += avail;
			len -= avail;
			s.inputData.shift();
			s.inputPos = 0;
		}
		return pos - startPos;
	}
}

@:allow(sys.net.SocketInput)
class Socket {
	var s:js.node.net.Socket;
	var inputData:Array<js.node.Buffer> = [];
	var inputPos:Int = 0;

	public var input:haxe.io.Input;

	public function new() {
		input = new SocketInput(this);
	}

	public function connect(host:Host, port:Int) {
		s = new js.node.net.Socket();
		s.on("data", function(buf:js.node.Buffer) inputData.push(buf));
		NodeSync.callVoid(function(callb) s.connect(port, host.host, callb));
	}

	function waitData() {
		if (inputData.length == 0)
			NodeSync.wait(function() return inputData.length > 0);
	}

	public function close() {
		if (s != null) {
			s.destroy();
			s = null;
		}
	}
}
