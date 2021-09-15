import haxe.io.Bytes;
import haxe.PosInfos;
import cpp.uv.UVError;
import haxe.Timer;
import cpp.uv.UVException;
import sys.thread.Thread;
import cpp.uv.Tcp;
import cpp.uv.SockAddr;

class TcpSample extends UVSample {
	static inline var PORT = 22001;

	public function run() {
		server();
		Timer.delay(client,100);
	}

	function handle(success:()->Void, ?p:PosInfos):(e:UVError)->Void {
		return e -> switch e {
			case UV_NOERR: success();
			case _: throw new UVException(e, p.fileName + ':' + p.lineNumber + ': ' + e.toString());
		}
	}

	function shutdownAndClose(tcp:Tcp, print:(msg:String)->Void, ?onClose:()->Void) {
		tcp.shutdown(_ -> {
			tcp.close(() -> {
				print('connection closed');
				if(onClose != null)
					onClose();
			});
		});
	}

	function server() {
		function print(msg:String) {
			this.print('SERVER: $msg');
		}
		var loop = Thread.current().events;
		var server = Tcp.init(loop, INET);
		server.bind(Ip4Addr('0.0.0.0', PORT));
		server.listen(32, handle(() -> {
			var client = Tcp.init(loop);
			server.accept(client);
			print('connection from ' + client.getSockName());
			client.readStart((e, data, bytesRead) -> switch e {
				case UV_NOERR:
					print('incoming request: ' + data.toBytes(bytesRead).toString());
					client.write(data, bytesRead, handle(() -> {
						shutdownAndClose(client, print, () -> {
							server.close(() -> print('done'));
						});
					}));
				case UV_EOF:
					print('client disconnected');
					client.close(() -> print('connection closed'));
				case _:
					throw new UVException(e);
			});
		}));
	}

	function client() {
		function print(msg:String) {
			this.print('CLIENT: $msg');
		}
		var loop = Thread.current().events;
		var client = Tcp.init(loop, INET);
		client.connect(Ip4Addr('127.0.0.1', PORT), handle(() -> {
			print('connected to ' + client.getPeerName());
			var data = Bytes.ofString('Hello, world!').getData();
			client.write(data.bytes, data.length, handle(() -> {
				client.readStart((e, data, bytesRead) -> switch e {
					case UV_NOERR:
						print('response from server: ' + data.toBytes(bytesRead).toString());
					case UV_EOF:
						print('disconnected from server');
						shutdownAndClose(client, print);
					case _:
						throw new UVException(e);
				});
			}));
		}));
	}
}