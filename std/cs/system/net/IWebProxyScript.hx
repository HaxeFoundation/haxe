package cs.system.net;

/** Provides the base interface to load and execute scripts for automatic proxy detection. */
@:native("System.Net.IWebProxyScript")
extern interface IWebProxyScript {
	/** Closes a script. */
	function Close():Void;
	/**
	 * Loads a script.
	 * @param scriptLocation Internal only.
	 * @param script Internal only.
	 * @param helperType Internal only.
	 * @return A  indicating whether the script was successfully loaded.
	 */
	function Load(scriptLocation:cs.system.Uri, script:String, helperType:cs.system.Type):Bool;
	/**
	 * Runs a script.
	 * @param url Internal only.
	 * @param host Internal only.
	 * @return A . An internal-only value returned.
	 */
	function Run(url:String, host:String):String;
}
