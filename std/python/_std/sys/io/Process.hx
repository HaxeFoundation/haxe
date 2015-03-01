
package sys.io;

import python.io.IoTools;
import python.lib.io.BufferedReader;
import python.lib.io.BufferedWriter;
import python.lib.io.TextIOWrapper;
import python.lib.Subprocess;
import python.lib.subprocess.Popen;

class Process {

	public var stdout(default,null) : haxe.io.Input;
	public var stderr(default,null) : haxe.io.Input;
	public var stdin(default,null) : haxe.io.Output;

	var p:Popen;

	public function new( cmd : String, args : Array<String> ) : Void {

		p = Popen.create([cmd].concat(args), { stdin : Subprocess.PIPE, stdout: Subprocess.PIPE, stderr : Subprocess.PIPE });

		this.stdout = IoTools.createFileInputFromText(new TextIOWrapper(new BufferedReader(p.stdout)));
		this.stderr = IoTools.createFileInputFromText(new TextIOWrapper(new BufferedReader(p.stderr)));
		this.stdin =  IoTools.createFileOutputFromText(new TextIOWrapper(new BufferedWriter(p.stdin)));
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