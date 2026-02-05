package cs.system.componentmodel;

/** Specifies that this object supports a simple, transacted notification for batch initialization. */
@:native("System.ComponentModel.ISupportInitialize")
extern interface ISupportInitialize {
	/** Signals the object that initialization is starting. */
	function BeginInit():Void;
	/** Signals the object that initialization is complete. */
	function EndInit():Void;
}
