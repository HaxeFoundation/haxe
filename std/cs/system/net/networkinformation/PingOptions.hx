package cs.system.net.networkinformation;

/** Used to control how  data packets are transmitted. */
@:native("System.Net.NetworkInformation.PingOptions")
extern class PingOptions {
	/**
	 * Gets or sets a  value that controls fragmentation of the data sent to the remote
	 * host.
	 * @return if the data cannot be sent in multiple packets; otherwise . The default
	 * is .
	 */
	var DontFragment(default, default):Bool;
	/**
	 * Gets or sets the number of routing nodes that can forward the  data before it is
	 * discarded.
	 * @return An  value that specifies the number of times the  data packets can be
	 * forwarded. The default is 128.
	 */
	var Ttl(default, default):Int;
	@:overload(function():Void {})
	function new(ttl:Int, dontFragment:Bool):Void;
}
