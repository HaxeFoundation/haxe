package cs.system;

@:native("System.IObserver")
extern interface IObserver<T> {
	function OnCompleted():Void;
	function OnError(error:cs.system.Exception):Void;
	function OnNext(value:T):Void;
}
