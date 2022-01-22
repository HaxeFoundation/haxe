import haxe.PosInfos;
import haxe.Timer;
import haxe.io.Bytes;
import cpp.uv.UVException;
import cpp.uv.SockAddr;
import cpp.uv.Udp;
import sys.thread.Thread;

class UdpSample extends UVSample {
	static inline var PORT = 22002;

	public function run() {
		server();
		Timer.delay(client,100);
	}

	function server() {
		function print(msg:String, ?p:PosInfos) {
			this.print('RECEIVER: $msg', p);
		}
		var loop = Thread.current().events;
		var udp = Udp.init(loop, INET, true);
		udp.bind(SockAddr.ipv4('0.0.0.0', PORT));
		var cnt = 0;
		udp.recvStart((e, data, bytesRead, addr, flags) -> switch e {
			case UV_NOERR:
				var msg = data.toString();
				if(bytesRead > 0) {
					print('Received message from $addr: $msg');
				} else {
					print('recv callback invocation with addr = $addr');
					if(addr == null && !flags.mmsgChunk)
						udp.close(() -> print('Done'));
				}
				var o = {
					mmsgChunk: flags.mmsgChunk,
					mmsgFree: flags.mmsgFree,
					partial: flags.partial,
				}
				print('...with flags $o');
			case _:
				throw new UVException(e);
		});
	}

	function client() {
		function print(msg:String, ?p:PosInfos) {
			this.print('SENDER: $msg', p);
		}
		var udp = Udp.init(Thread.current().events, INET);
		var data = Bytes.ofString('Hello, UDP!');
		udp.send(data, 0, data.length, SockAddr.ipv4('127.0.0.1', PORT), e -> switch e {
			case UV_NOERR:
				print('Message sent');
				udp.close(() -> print('Done'));
			case _:
				throw new UVException(e);
		});
	}
}