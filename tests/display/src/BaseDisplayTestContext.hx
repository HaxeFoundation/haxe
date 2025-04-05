import haxe.io.Bytes;
import haxe.io.Path;

using StringTools;

import Types;

class BaseDisplayTestContext {
	static var haxeServer = {
		var hx = "haxe";
		var testHaxe = haxe.macro.Compiler.getDefine("test_haxe");
		if (testHaxe != null) {
			Sys.putEnv("HAXE_STD_PATH", '${Path.directory(testHaxe)}/std');
			hx = testHaxe;
		}

		haxeserver.HaxeServerSync.launch(hx, []);
	}

	var dir:String = ".";
	var vfs:Vfs;
	var markers:Map<Int, Int>;
	var fieldName:String;

	public var target:Null<haxe.macro.Compiler.Platform> = Cross;
	public final source:File;

	public function new(path:String, fieldName:String, source:String, markers:Map<Int, Int>) {
		var test = new Path(path).file;
		this.dir = '${Sys.getCwd()}/test/cases/${test}';
		this.vfs = new Vfs(dir);
		var path = Path.withoutDirectory(path);
		vfs.putContent(path, source);

		this.fieldName = fieldName;
		this.source = new File(path, source);
		this.markers = markers;
	}

	public function pos(id:Int):Position {
		var r = markers[id];
		if (r == null)
			throw "No such marker: " + id;
		return new Position(r);
	}

	public function range(pos1:Int, pos2:Int) {
		return normalizePath(source.formatRange(pos(pos1), pos(pos2)));
	}

	public function hasErrorMessage(f:()->Void, message:String) {
		return try {
			f();
			false;
		} catch (exc:HaxeInvocationException) {
			return exc.message.indexOf(message) != -1;
		}
	}

	public function runHaxe(args:Array<String>, ?stdin:String) {
		args = switch (target) {
			case Cross: args;
			case Js: ["--js", "out.js"].concat(args);
			case Lua: ["--lua", "out.lua"].concat(args);
			case Neko: ["--neko", "out.n"].concat(args);
			case Flash: ["--swf", "out.swf"].concat(args);
			case Php: ["--php", "php-out"].concat(args);
			case Cpp: ["--cpp", "cpp-out"].concat(args);
			case Jvm: ["--jvm", "out.jar"].concat(args);
			case Python: ["--python", "out.py"].concat(args);
			case Hl: ["--hl", "out.hl"].concat(args);
			case Eval: ["--interp"].concat(args);
			case CustomTarget(name): ["--custom-target", name].concat(args);
		}

		return haxeServer.rawRequest([
			"--cwd", dir,
			"-cp", '${Sys.getCwd()}/src-misc',
			"-D", "message.reporting=classic",
			"--no-output"
		].concat(args), stdin == null ? null : Bytes.ofString(stdin));
	}

	function normalizePath(p:String):String {
		if (!haxe.io.Path.isAbsolute(p)) {
			p = vfs.getPhysicalPath(p).toString();
		}
		if (Sys.systemName() == "Windows") {
			// on windows, haxe returns paths with backslashes, drive letter uppercased
			p = p.substr(0, 1).toUpperCase() + p.substr(1);
			p = p.replace("/", "\\");
		}
		return p;
	}
}
