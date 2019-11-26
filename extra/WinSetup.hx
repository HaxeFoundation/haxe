import sys.io.Process;

using haxe.io.Path;
using StringTools;
using sys.FileSystem;
using WinSetup;

enum abstract RegDataType<T>(String) to String {
	var REG_EXPAND_SZ:RegDataType<String>;
	var REG_SZ:RegDataType<String>;
}

class WinSetup {
	static inline var HAXEPATH = 'HAXEPATH';
	static inline var NEKO_INSTPATH = 'NEKO_INSTPATH';

	static inline var REG_ENVIRONMENT = 'HKEY_LOCAL_MACHINE\\SYSTEM\\CurrentControlSet\\Control\\Session Manager\\Environment';

	static function main() {
		try run()
		catch(e:Dynamic) {
			Sys.stderr().writeString(Std.string(e) + '\n');
			#if debug
				Sys.stderr().writeString(haxe.CallStack.toString(haxe.CallStack.exceptionStack()) + '\n');
			#end
			Sys.stderr().flush();
			#if debug
			Sys.println('Press any key to exit...');
			Sys.getChar(false);
			#end
			Sys.exit(1);
		}
	}

	static function envVar(name:String):String {
		return '%$name%';
	}

	static function run() {
		var haxePath = Sys.getCwd().removeTrailingSlashes();
		var addHaxe = '$haxePath\\haxe.exe'.exists();
		if(addHaxe) {
			setVar(HAXEPATH, REG_SZ, haxePath);
		}

		var nekoPath = Path.join([Path.directory(haxePath), 'neko']).replace('/', '\\');
		var addNeko = '$nekoPath\\neko.exe'.exists();
		if(addNeko) {
			setVar(NEKO_INSTPATH, REG_SZ, nekoPath);
		}

		if(!addHaxe && !addNeko) {
			return;
		}

		var paths = readPath().split(';');
		addHaxe = paths.indexOf(HAXEPATH.envVar()) < 0 && addHaxe;
		if(addHaxe) {
			paths.push(HAXEPATH.envVar());
		}
		addNeko = paths.indexOf(NEKO_INSTPATH.envVar()) < 0 && addNeko;
		if(addNeko) {
			paths.push(NEKO_INSTPATH.envVar());
		}
		if(addHaxe || addNeko) {
			setVar('path', REG_EXPAND_SZ, paths.join(';'));
		}
	}

	static function readPath():String {
		var p = new Process('reg', ['query', REG_ENVIRONMENT, '/v', 'path']);
		if(p.exitCode() != 0) {
			var error = p.stderr.readAll().toString();
			p.close();
			throw 'Cannot query reg.exe for PATH:\n$error';
		}
		/**
		 * Sample response:
		 *
		 *	HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Control\Session Manager\Environment
		 *	    path    REG_EXPAND_SZ    %SystemRoot%\system32;%SystemRoot%;%SystemRoo<...>
		 */
		var response = p.stdout.readAll().toString();
		p.close();
		var lines = response.split('\n');
		for(line in lines) {
			line = line.trim();
			if(line.substr(0, 'path'.length).toLowerCase() == 'path') {
				var column = 0;
				var wasSpace = false;
				for(pos in 0...line.length) {
					var isSpace = line.isSpace(pos);
					if(wasSpace && !isSpace) {
						column++;
						if(column == 2) {
							return line.substr(pos);
						}
					}
					wasSpace = isSpace;
				}
			}
		}
		throw 'Cannot parse a query to reg.exe for PATH value:\n$response';
	}

	static function setVar<T>(name:String, dataType:RegDataType<T>, value:T) {
		var p = new Process('reg', ['add', REG_ENVIRONMENT, '/v', name, '/t', dataType, '/d', '$value', '/f']);
		if(p.exitCode() != 0) {
			var error = p.stderr.readAll().toString();
			p.close();
			throw 'Cannot set a value for $name via reg.exe:\n$error';
		}
	}
}