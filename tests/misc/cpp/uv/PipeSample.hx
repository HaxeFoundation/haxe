import sys.FileSystem;
import cpp.uv.Misc;
import cpp.uv.SockAddr;
import cpp.uv.Tcp;
import haxe.io.Bytes;
import cpp.uv.Pipe;
import cpp.uv.UVException;
import cpp.uv.UVError;
import haxe.PosInfos;
import sys.thread.Thread;
import haxe.Timer;

class PipeSample extends UVSample {
	static var NAME = 'testpipe';

	public function run() {
		NAME = Misc.tmpDir() + '/' + NAME;
		print('Running PipeSample server...');
		print('waiting for connections');
		server();
		haxe.Timer.delay(() -> {
			print('Running PipeSample client...');
			client();
		}, 200);
	}

	static function handle(success:()->Void, ?p:PosInfos):(e:UVError)->Void {
		return e -> switch e {
			case UV_NOERR: success();
			case _: throw new UVException(e, p.fileName + ':' + p.lineNumber + ': ' + e.toString());
		}
	}

	function server() {
		if(FileSystem.exists(NAME))
			FileSystem.deleteFile(NAME);
		function print(msg:String, ?p:PosInfos)
			this.print('SERVER: $msg', p);
		var loop = Thread.current().events;
		var server = Pipe.init(loop);
		server.bind(NAME);
		server.listen(32, handle(() -> {
			var client = Pipe.init(loop, true);
			server.accept(client);
			print('connection from ' + client.getSockName());
			var tcp = Tcp.init(loop, INET);
			client.readStart((e, data, bytesRead) -> switch e {
				case UV_NOERR:
					print('incoming request: ' + data.toString());
					var addr = SockAddr.ipv4('93.184.216.34', 80); //http://example.com
					tcp.connect(addr, handle(() -> {
						print('tcp connected to ' + addr);
						client.write2(data, 0, bytesRead, tcp, handle(() -> print('tcp sent')));
					}));
				case UV_EOF:
					print('client disconnected');
					tcp.close(() -> {
						print('tcp closed');
						client.close(() -> {
							print('pipe connection closed');
							server.close(() -> print('done'));
						});
					});
				case _:
					throw new UVException(e);
			});
		}));
	}

	function client() {
		function print(msg:String, ?p:PosInfos)
			this.print('CLIENT: $msg', p);
		var loop = Thread.current().events;
		var client = Pipe.init(loop, true);
		client.connect(NAME, handle(() -> {
			print('connected to ' + client.getPeerName());
			var data = Bytes.ofString('Hello, world!');
			client.write(data, 0, data.length, handle(() -> {
				var tcp = Tcp.init(loop);
				client.readStart((e, data, bytesRead) -> switch e {
					case UV_NOERR:
						while(client.pendingCount() > 0) {
							switch client.pendingType() {
								case TCP:
									client.accept(tcp);
									print('Received tcp socket connected to ' + tcp.getPeerName());
								case _:
									throw 'Received unexpected handler type';
							}
						}
						print('response from server: ' + data.toString());
						client.close(() -> print('pipe connection closed'));
					case UV_EOF:
						print('disconnected from server');
						client.close(() -> print('pipe connection closed'));
					case _:
						throw new UVException(e);
				});
			}));
		}));
	}
}