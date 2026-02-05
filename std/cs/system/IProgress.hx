package cs.system;

@:native("System.IProgress")
extern interface IProgress<T> {
	function Report(value:T):Void;
}
