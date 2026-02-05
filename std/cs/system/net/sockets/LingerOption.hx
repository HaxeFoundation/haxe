package cs.system.net.sockets;

/** Specifies whether a  will remain connected after a call to the  or  methods and the length of time it will remain connected, if data remains to be sent. */
@:native("System.Net.Sockets.LingerOption")
extern class LingerOption {
	/**
	 * Gets or sets a value that indicates whether to linger after the  is closed.
	 * @return if the  should linger after  is called; otherwise, .
	 */
	var Enabled(default, default):Bool;
	/**
	 * Gets or sets the amount of time to remain connected after calling the  method if
	 * data remains to be sent.
	 * @return The amount of time, in seconds, to remain connected after calling .
	 */
	var LingerTime(default, default):Int;
	function new(enable:Bool, seconds:Int):Void;
}
