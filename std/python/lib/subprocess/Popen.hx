
package python.lib.subprocess;

import python.lib.io.BufferedReader;
import python.lib.io.TextIOBase;
import python.lib.Subprocess.StartupInfo;
import python.lib.Tuple;

typedef PopenOptions = {
	?bufsize : Int,
	?executable  : String,
	?stdin  : Dynamic,
	?stdout  : Dynamic,
	?stderr : Dynamic,
	?preexec_fn : Void->Void,
	?close_fds : Bool,
	?shell : Bool,
	?cwd : String,
	?env : Dict<String, String>,
	?universal_newlines : Bool,
	?startupinfo : StartupInfo,
	?creationflags : Int,
}

@:pythonImport("subprocess", "Popen")
extern class Popen {

	public static inline function create (args:Array<String>, o:PopenOptions):Popen {

		o.bufsize = if (Reflect.hasField(o, "bufsize")) o.bufsize else 0;
		o.executable = if (Reflect.hasField(o, "executable")) o.executable else null;
		o.stdin = if (Reflect.hasField(o, "stdin")) o.stdin else null;
		o.stdout = if (Reflect.hasField(o, "stdout")) o.stdout else null;
		o.stderr = if (Reflect.hasField(o, "stderr")) o.stderr else null;
		o.preexec_fn = if (Reflect.hasField(o, "preexec_fn")) o.preexec_fn else null;
		o.close_fds = if (Reflect.hasField(o, "close_fds")) o.close_fds else null;
		o.shell = if (Reflect.hasField(o, "shell")) o.shell else null;
		o.cwd = if (Reflect.hasField(o, "cwd")) o.cwd else null;
		o.env = if (Reflect.hasField(o, "env")) o.env else null;
		o.universal_newlines = if (Reflect.hasField(o, "universal_newlines")) o.universal_newlines else null;
		o.startupinfo = if (Reflect.hasField(o, "startupinfo")) o.startupinfo else null;

		o.creationflags = if (Reflect.hasField(o, "creationflags")) o.creationflags else 0;

		if (std.Sys.systemName() == "Windows") {
			return new Popen(args, o.bufsize, o.executable, o.stdin, o.stdout, o.stderr, o.preexec_fn,
			o.close_fds, o.shell, o.cwd, o.env, o.universal_newlines, o.startupinfo, o.creationflags);
		} else {
			return new Popen(args, o.bufsize, o.executable, o.stdin, o.stdout, o.stderr, o.preexec_fn,
			o.close_fds, o.shell, o.cwd, o.env, o.universal_newlines, o.startupinfo);
		}

	}

	public function new (args:Array<String>, bufsize:Int=0, executable:String = null,
			stdin:Int = null, stdout:Int = null, stderr:Int=null, preexec_fn:Void->Void=null,
			close_fds:Bool=false, shell:Bool=false, cwd:String=null, env:Dict<String,String>=null,
			universal_newlines:Bool=false, startupinfo:StartupInfo=null, creationflags:Int=0):Void;




	public function kill ():Void;
	public function wait (?timeout:Null<Int>):Null<Int>;
	public function poll ():Null<Int>;
	public function terminate ():Void;

	public var stdout : BufferedReader;
	public var stderr : BufferedReader;
	public var stdin : BufferedReader;
	public var returncode : Int;
	public var pid:Int;

	public function communicate (input:Bytes = null, timeout:Null<Int> = null):Tup2<Bytes, Bytes>;

}