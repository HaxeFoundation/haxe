import cpp.uv.SockAddr;
import cpp.uv.UVException;
import sys.thread.Thread;
import cpp.uv.Dns;

class DnsSample extends UVSample {
	public function run() {
		var loop = Thread.current().events;
		Dns.getAddrInfo(loop, 'haxe.org', 'http',
			{ flags: [AIF_CANONNAME], family: INET },
			(e, infos) -> switch e {
				case UV_NOERR:
					for(i in infos) {
						print('getAddrInfo: addr ${i.addr}, canonname ${i.canonName}');
						if(i.canonName != null) {
							Dns.getNameInfo(loop, i.addr, [NIF_NAMEREQD],
								(e, name, service) -> switch e {
									case UV_NOERR:
										print('getNameInfo: host $name, service $service');
									case _:
										throw new UVException(e);
								}
							);
						}
					}
				case _:
					throw new UVException(e);
			}
		);
	}
}