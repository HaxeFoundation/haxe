
package sys.io;

import python.lib.Subprocess;
import python.lib.subprocess.Popen;

class Process {

	public var stdout(default,null) : haxe.io.Input;
	public var stderr(default,null) : haxe.io.Input;
	public var stdin(default,null) : haxe.io.Output;

	var p:Popen;

	public function new( cmd : String, args : Array<String> ) : Void {

		p = Popen.create([cmd].concat(args), { stdin : Subprocess.PIPE, stdout: Subprocess.PIPE, stderr : Subprocess.PIPE });


		this.stdout = new FileInput (cast p.stdout);
		this.stderr = new FileInput (cast p.stderr);
		this.stdin =  new FileOutput(cast p.stdin);
	}

	public function getPid() : Int {
		return p.pid;
	}
	public function exitCode() : Int {
		return p.returncode;
	}
	public function close() : Void {
		p.terminate();
	}
	public function kill() : Void {
		p.kill();
	}

}