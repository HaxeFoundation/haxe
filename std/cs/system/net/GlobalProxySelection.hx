package cs.system.net;

/** Contains a global default proxy instance for all HTTP requests. */
@:native("System.Net.GlobalProxySelection")
extern class GlobalProxySelection {
	/**
	 * Gets or sets the global HTTP proxy.
	 * @return An  that every call to  uses.
	 */
	static var Select(default, default):cs.system.net.IWebProxy;
	function new():Void;
	/**
	 * Returns an empty proxy instance.
	 * @return An  that contains no information.
	 */
	static function GetEmptyWebProxy():cs.system.net.IWebProxy;
}
