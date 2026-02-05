package cs.system;

/** Provides a mechanism for releasing unmanaged resources. */
@:native("System.IDisposable")
extern interface IDisposable {
	/** Performs application-defined tasks associated with freeing, releasing, or resetting unmanaged resources. */
	function Dispose():Void;
}
