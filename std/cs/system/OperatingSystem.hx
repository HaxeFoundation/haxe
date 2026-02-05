package cs.system;

/** Represents information about an operating system, such as the version and platform identifier. This class cannot be inherited. */
@:native("System.OperatingSystem")
extern class OperatingSystem {
	/**
	 * Gets a  enumeration value that identifies the operating system platform.
	 * @return One of the  values.
	 */
	var Platform(default, never):cs.system.PlatformID;
	/**
	 * Gets the service pack version represented by this  object.
	 * @return The service pack version, if service packs are supported and at least
	 * one is installed; otherwise, an empty string ("").
	 */
	var ServicePack(default, never):String;
	/**
	 * Gets a  object that identifies the operating system.
	 * @return A  object that describes the major version, minor version, build, and
	 * revision numbers for the operating system.
	 */
	var Version(default, never):cs.system.Version;
	/**
	 * Gets the concatenated string representation of the platform identifier, version,
	 * and service pack that are currently installed on the operating system.
	 * @return The string representation of the values returned by the , , and 
	 * properties.
	 */
	var VersionString(default, never):String;
	function new(platform:cs.system.PlatformID, version:cs.system.Version):Void;
	/**
	 * Creates an  object that is identical to this instance.
	 * @return An  object that is a copy of this instance.
	 */
	function Clone():Dynamic;
	/**
	 * Populates a  object with the data necessary to deserialize this instance.
	 * @param info The object to populate with serialization information.
	 * @param context The place to store and retrieve serialized data. Reserved for
	 * future use.
	 */
	function GetObjectData(info:cs.system.runtime.serialization.SerializationInfo, context:cs.system.runtime.serialization.StreamingContext):Void;
	/**
	 * Converts the value of this  object to its equivalent string representation.
	 * @return The string representation of the values returned by the , , and 
	 * properties.
	 */
	function ToString():String;
}
