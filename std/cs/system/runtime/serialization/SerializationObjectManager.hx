package cs.system.runtime.serialization;

/** Manages serialization processes at run time. This class cannot be inherited. */
@:native("System.Runtime.Serialization.SerializationObjectManager")
extern class SerializationObjectManager {
	function new(context:cs.system.runtime.serialization.StreamingContext):Void;
	/** Invokes the OnSerializing callback event if the type of the object has one; and registers the object for raising the OnSerialized event if the type of the object has one. */
	function RaiseOnSerializedEvent():Void;
	/**
	 * Registers the object upon which events will be raised.
	 * @param obj The object to register.
	 */
	function RegisterObject(obj:Dynamic):Void;
}
