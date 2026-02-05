package cs.system.runtime.serialization;

/** Keeps track of objects as they are deserialized. */
@:native("System.Runtime.Serialization.ObjectManager")
extern class ObjectManager {
	function new(selector:cs.system.runtime.serialization.ISurrogateSelector, context:cs.system.runtime.serialization.StreamingContext):Void;
	/** Performs all the recorded fixups. */
	function DoFixups():Void;
	/**
	 * Returns the object with the specified object ID.
	 * @param objectID The ID of the requested object.
	 * @return The object with the specified object ID if it has been previously stored
	 * or  if no such object has been registered.
	 */
	function GetObject(objectID:haxe.Int64):Dynamic;
	/** Raises the deserialization event to any registered object that implements . */
	function RaiseDeserializationEvent():Void;
	/**
	 * Invokes the method marked with the .
	 * @param obj The instance of the type that contains the method to be invoked.
	 */
	function RaiseOnDeserializingEvent(obj:Dynamic):Void;
	@:overload(function(arrayToBeFixed:haxe.Int64, index:Int, objectRequired:haxe.Int64):Void {})
	/**
	 * Records a fixup for one element in an array.
	 * @param arrayToBeFixed The ID of the array used to record a fixup.
	 * @param index The index within arrayFixup that a fixup is requested for.
	 * @param objectRequired The ID of the object that the current array element will
	 * point to after fixup is completed.
	 */
	function RecordArrayElementFixup(arrayToBeFixed:haxe.Int64, indices:cs.NativeArray<Int>, objectRequired:haxe.Int64):Void;
	/**
	 * Records a fixup for an object member, to be executed later.
	 * @param objectToBeFixed The ID of the object that needs the reference to .
	 * @param memberName The member name of  where the fixup will be performed.
	 * @param objectRequired The ID of the object required by .
	 */
	function RecordDelayedFixup(objectToBeFixed:haxe.Int64, memberName:String, objectRequired:haxe.Int64):Void;
	/**
	 * Records a fixup for a member of an object, to be executed later.
	 * @param objectToBeFixed The ID of the object that needs the reference to the 
	 * object.
	 * @param member The member of  where the fixup will be performed.
	 * @param objectRequired The ID of the object required by .
	 */
	function RecordFixup(objectToBeFixed:haxe.Int64, member:cs.system.reflection.MemberInfo, objectRequired:haxe.Int64):Void;
	@:overload(function(obj:Dynamic, objectID:haxe.Int64):Void {})
	@:overload(function(obj:Dynamic, objectID:haxe.Int64, info:cs.system.runtime.serialization.SerializationInfo):Void {})
	@:overload(function(obj:Dynamic, objectID:haxe.Int64, info:cs.system.runtime.serialization.SerializationInfo, idOfContainingObj:haxe.Int64, member:cs.system.reflection.MemberInfo):Void {})
	/**
	 * Registers an object as it is deserialized, associating it with .
	 * @param obj The object to register.
	 * @param objectID The ID of the object to register.
	 */
	function RegisterObject(obj:Dynamic, objectID:haxe.Int64, info:cs.system.runtime.serialization.SerializationInfo, idOfContainingObj:haxe.Int64, member:cs.system.reflection.MemberInfo, arrayIndex:cs.NativeArray<Int>):Void;
}
