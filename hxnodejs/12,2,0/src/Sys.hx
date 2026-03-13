import haxe.io.Bytes;
import haxe.io.Eof;
import haxe.io.Error;
import haxe.io.Output;
import js.Node.process;
import js.node.Buffer;
import js.node.ChildProcess;
import js.node.Fs;

@:dce
@:coreApi
class Sys {
	public static inline function print(v:Dynamic):Void {
		process.stdout.write(Std.string(v));
	}

	public static inline function println(v:Dynamic):Void {
		process.stdout.write(Std.string(v));
		process.stdout.write("\n");
	}

	public static inline function args():Array<String> {
		return process.argv.slice(2);
	}

	public static inline function getEnv(s:String):String {
		return process.env[s];
	}

	public static inline function putEnv(s:String, v:Null<String>):Void {
		if (v == null)
			process.env.remove(s);
		else
			process.env[s] = v;
	}

	public static function environment():Map<String, String> {
		var m = new Map();
		for (key in process.env.keys())
			m[key] = process.env[key];
		return m;
	}

	public inline static function setTimeLocale(loc:String):Bool {
		return false;
	}

	public inline static function getCwd():String {
		return haxe.io.Path.addTrailingSlash(process.cwd());
	}

	public static inline function setCwd(s:String):Void {
		process.chdir(s);
	}

	public static function systemName():String {
		return switch (process.platform) {
			case "darwin": "Mac";
			case "freebsd": "BSD";
			case "linux": "Linux";
			case "win32": "Windows";
			case other: other; // throw?
		}
	}

	public static inline function command(cmd:String, ?args:Array<String>):Int {
		if (args == null)
			return ChildProcess.spawnSync(cmd, {shell: true, stdio: "inherit"}).status;
		else
			return ChildProcess.spawnSync(cmd, args, {stdio: "inherit"}).status;
	}

	public static inline function exit(code:Int):Void {
		process.exit(code);
	}

	public static inline function time():Float {
		return (cast Date).now() / 1000;
	}

	public static inline function cpuTime():Float {
		return process.uptime();
	}

	#if (haxe_ver >= 3.3)
	@:deprecated("Use programPath instead")
	#end
	public static inline function executablePath():String {
		return process.argv[0];
	}

	#if (haxe_ver >= 3.3)
	public static inline function programPath():String {
		return js.Node.__filename;
	}
	#end

	public static function getChar(echo:Bool):Int {
		throw "Sys.getChar is currently not implemented on node.js";
	}

	public static function sleep(seconds:Float):Void {
		var end = (cast Date).now() + seconds * 1000;
		while ((cast Date).now() <= end) {}
	}

	public static inline function stdin():haxe.io.Input {
		return new FileInput(0);
	}

	public static inline function stdout():haxe.io.Output {
		return new FileOutput(1);
	}

	public static inline function stderr():haxe.io.Output {
		return new FileOutput(2);
	}
}

private class FileOutput extends haxe.io.Output {
	var fd:Int;

	public function new(fd:Int) {
		this.fd = fd;
	}

	override public function writeByte(c:Int) {
		Fs.writeSync(fd, String.fromCharCode(c));
	}

	override public function writeBytes(s:Bytes, pos:Int, len:Int):Int {
		return Fs.writeSync(fd, Buffer.hxFromBytes(s), pos, len);
	}

	override public function writeString(s:String #if (haxe_ver >= 4), ?encoding:haxe.io.Encoding #end) {
		Fs.writeSync(fd, s);
	}

	override public function flush() {
		Fs.fsyncSync(fd);
	}

	override public function close() {
		Fs.closeSync(fd);
	}
}

private class FileInput extends haxe.io.Input {
	var fd:Int;

	public function new(fd:Int) {
		this.fd = fd;
	}

	override public function readByte():Int {
		var buf = Buffer.alloc(1);
		try {
			Fs.readSync(fd, buf, 0, 1, null);
		} catch (e:Dynamic) {
			if (e.code == "EOF")
				throw new Eof();
			else
				throw Error.Custom(e);
		}
		return buf[0];
	}

	override public function readBytes(s:Bytes, pos:Int, len:Int):Int {
		var buf = Buffer.hxFromBytes(s);
		try {
			return Fs.readSync(fd, buf, pos, len, null);
		} catch (e:Dynamic) {
			if (e.code == "EOF")
				throw new Eof();
			else
				throw Error.Custom(e);
		}
	}

	override public function close():Void {
		Fs.closeSync(fd);
	}
}
