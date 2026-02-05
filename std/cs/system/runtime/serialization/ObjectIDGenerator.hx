package cs.system.runtime.serialization;

/** Generates IDs for objects. */
@:native("System.Runtime.Serialization.ObjectIDGenerator")
extern class ObjectIDGenerator {
	function new():Void;
	/**
	 * Returns the ID for the specified object, generating a new ID if the specified
	 * object has not already been identified by the .
	 * @param obj The object you want an ID for.
	 * @param firstTime if  was not previously known to the ; otherwise, .
	 * @return The object's ID is used for serialization.  is set to  if this is the
	 * first time the object has been identified; otherwise, it is set to .
	 */
	function GetId(obj:Dynamic, firstTime:cs.Ref<Bool>):haxe.Int64;
	/**
	 * Determines whether an object has already been assigned an ID.
	 * @param obj The object you are asking for.
	 * @param firstTime if  was not previously known to the ; otherwise, .
	 * @return The object ID of  if previously known to the ; otherwise, zero.
	 */
	function HasId(obj:Dynamic, firstTime:cs.Ref<Bool>):haxe.Int64;
}
