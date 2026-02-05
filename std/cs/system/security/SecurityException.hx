package cs.system.security;

/** The exception that is thrown when a security error is detected. */
@:native("System.Security.SecurityException")
extern class SecurityException extends cs.system.SystemException {
	/**
	 * Gets or sets the demanded security permission, permission set, or permission set
	 * collection that failed.
	 * @return A permission, permission set, or permission set collection object.
	 */
	var Demanded(default, default):Dynamic;
	/**
	 * Gets or sets the denied security permission, permission set, or permission set
	 * collection that caused a demand to fail.
	 * @return A permission, permission set, or permission set collection object.
	 */
	var DenySetInstance(default, default):Dynamic;
	/**
	 * Gets or sets information about the failed assembly.
	 * @return An  that identifies the failed assembly.
	 */
	var FailedAssemblyInfo(default, default):cs.system.reflection.AssemblyName;
	/**
	 * Gets or sets the granted permission set of the assembly that caused the .
	 * @return The XML representation of the granted set of the assembly.
	 */
	var GrantedSet(default, default):String;
	/**
	 * Gets or sets the information about the method associated with the exception.
	 * @return A  object describing the method.
	 */
	var Method(default, default):cs.system.reflection.MethodInfo;
	/**
	 * Gets or sets the state of the permission that threw the exception.
	 * @return The state of the permission at the time the exception was thrown.
	 */
	var PermissionState(default, default):String;
	/**
	 * Gets or sets the type of the permission that failed.
	 * @return The type of the permission that failed.
	 */
	var PermissionType(default, default):cs.system.Type;
	/**
	 * Gets or sets the permission, permission set, or permission set collection that
	 * is part of the permit-only stack frame that caused a security check to fail.
	 * @return A permission, permission set, or permission set collection object.
	 */
	var PermitOnlySetInstance(default, default):Dynamic;
	/**
	 * Gets or sets the refused permission set of the assembly that caused the .
	 * @return The XML representation of the refused permission set of the assembly.
	 */
	var RefusedSet(default, default):String;
	/**
	 * Gets or sets the URL of the assembly that caused the exception.
	 * @return A URL that identifies the location of the assembly.
	 */
	var Url(default, default):String;
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	@:overload(function(message:String, inner:cs.system.Exception):Void {})
	@:overload(function(message:String, type:cs.system.Type):Void {})
	function new(message:String, type:cs.system.Type, state:String):Void;
	/**
	 * Sets the  with information about the .
	 * @param info The  that holds the serialized object data about the exception being
	 * thrown.
	 * @param context The  that contains contextual information about the source or
	 * destination.
	 */
	function GetObjectData(info:cs.system.runtime.serialization.SerializationInfo, context:cs.system.runtime.serialization.StreamingContext):Void;
	/**
	 * Returns a representation of the current .
	 * @return A string representation of the current .
	 */
	function ToString():String;
}
