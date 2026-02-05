package cs.system;

@:native("System.IObservable")
extern interface IObservable<T> {
	function Subscribe(observer:cs.system.IObserver<T>):cs.system.IDisposable;
}
