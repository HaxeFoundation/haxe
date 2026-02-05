package cs.system;

/** The exception that is thrown when there is an attempt to dynamically access a class member that does not exist or that is not declared as public. If a member in a class library has been removed or renamed, recompile any assemblies that reference that library. */
@:native("System.MissingMemberException")
extern class MissingMemberException extends cs.system.MemberAccessException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	@:overload(function(message:String, inner:cs.system.Exception):Void {})
	function new(className:String, memberName:String):Void;
	/**
	 * Sets the  object with the class name, the member name, the signature of the
	 * missing member, and additional exception information.
	 * @param info The object that holds the serialized object data.
	 * @param context The contextual information about the source or destination.
	 */
	function GetObjectData(info:cs.system.runtime.serialization.SerializationInfo, context:cs.system.runtime.serialization.StreamingContext):Void;
}
