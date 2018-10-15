package unit.issues;

class Issue7544 extends unit.Test {
	function test() {
		#if sys
		var filename = 'issue7544.txt';
		sys.io.File.saveContent(filename, '1');
		var target = sys.io.File.read(filename, true);
		var buf = haxe.io.Bytes.alloc(1);
		
		function next() {
			try {
				var read = target.readBytes(buf, 0, 1);
				return Std.string(read);
			} catch(e:haxe.io.Eof) {
				return Std.string('eof');
			} catch(e:Dynamic) {
				return Std.string(e);
			}
		}
		
		eq('1', next());
		next(); // TODO: at this line, some target produce '0', some produce 'eof', do we need to unify?
		eq('eof', next());
		sys.FileSystem.deleteFile(filename);
		#end
	}
}