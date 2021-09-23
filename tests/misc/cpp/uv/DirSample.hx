import cpp.uv.Misc;
import cpp.uv.Dir;
import cpp.uv.UVException;
import sys.thread.Thread;

class DirSample extends UVSample {

	public function run() {
		var path = Misc.tmpDir();
		var loop = Thread.current().events;
		print('ASYNC functions:');
		Dir.open(loop, path, (e, dir) -> switch e {
			case UV_NOERR:
				dir.read(loop, 3, (e, entries) -> switch e {
					case UV_NOERR:
						for(i in 0...entries.length)
							print('\t${entries[i]}');
						dir.close(loop, e -> switch e {
							case UV_NOERR: print('Done');
							case _: throw new UVException(e);
						});
					case _:
						throw new UVException(e);
				});
			case _:
				throw new UVException(e);
		});
	}
}