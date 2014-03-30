
package sys.io;

class Process {

	var stdout(default,null) : haxe.io.Input;
	var stderr(default,null) : haxe.io.Input;
	var stdin(default,null) : haxe.io.Output;

	function new( cmd : String, args : Array<String> ) : Void {
		throw "not implemented";
	}
	function getPid() : Int {
		return throw "not implemented";
	}
	function exitCode() : Int {
		return throw "not implemented";
	}
	function close() : Void {
		throw "not implemented";
	}
	function kill() : Void {
		throw "not implemented";
	}

}