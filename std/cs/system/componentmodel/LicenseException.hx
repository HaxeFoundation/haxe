package cs.system.componentmodel;

/** Represents the exception thrown when a component cannot be granted a license. */
@:native("System.ComponentModel.LicenseException")
extern class LicenseException extends cs.system.SystemException {
	/**
	 * Gets the type of the component that was not granted a license.
	 * @return A  that represents the type of component that was not granted a license.
	 */
	var LicensedType(default, never):cs.system.Type;
	@:overload(function(type:cs.system.Type):Void {})
	@:overload(function(type:cs.system.Type, instance:Dynamic):Void {})
	@:overload(function(type:cs.system.Type, instance:Dynamic, message:String):Void {})
	function new(type:cs.system.Type, instance:Dynamic, message:String, innerException:cs.system.Exception):Void;
	/**
	 * Sets the  with information about the exception.
	 * @param info The  to be used for deserialization.
	 * @param context The destination to be used for deserialization.
	 */
	function GetObjectData(info:cs.system.runtime.serialization.SerializationInfo, context:cs.system.runtime.serialization.StreamingContext):Void;
}
