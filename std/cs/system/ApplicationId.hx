package cs.system;

/** Contains information used to uniquely identify a manifest-based application. This class cannot be inherited. */
@:native("System.ApplicationId")
extern class ApplicationId {
	/**
	 * Gets a string representing the culture information for the application.
	 * @return The culture information for the application.
	 */
	var Culture(default, never):String;
	/**
	 * Gets the name of the application.
	 * @return The name of the application.
	 */
	var Name(default, never):String;
	/**
	 * Gets the target processor architecture for the application.
	 * @return The processor architecture of the application.
	 */
	var ProcessorArchitecture(default, never):String;
	/**
	 * Gets the public key token for the application.
	 * @return A byte array containing the public key token for the application.
	 */
	var PublicKeyToken(default, never):cs.NativeArray<cs.UInt8>;
	/**
	 * Gets the version of the application.
	 * @return A  that specifies the version of the application.
	 */
	var Version(default, never):cs.system.Version;
	function new(publicKeyToken:cs.NativeArray<cs.UInt8>, name:String, version:cs.system.Version, processorArchitecture:String, culture:String):Void;
	/**
	 * Creates and returns an identical copy of the current application identity.
	 * @return An  object that represents an exact copy of the original.
	 */
	function Copy():cs.system.ApplicationId;
	/**
	 * Determines whether the specified  object is equivalent to the current .
	 * @param o The  object to compare to the current .
	 * @return if the specified  object is equivalent to the current ; otherwise, .
	 */
	function Equals(o:Dynamic):Bool;
	/**
	 * Gets the hash code for the current application identity.
	 * @return The hash code for the current application identity.
	 */
	function GetHashCode():Int;
	/**
	 * Creates and returns a string representation of the application identity.
	 * @return A string representation of the application identity.
	 */
	function ToString():String;
}
