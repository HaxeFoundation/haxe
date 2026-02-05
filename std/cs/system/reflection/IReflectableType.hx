package cs.system.reflection;

/** Represents a type that you can reflect over. */
@:native("System.Reflection.IReflectableType")
extern interface IReflectableType {
	/**
	 * Retrieves an object that represents this type.
	 * @return An object that represents this type.
	 */
	function GetTypeInfo():cs.system.reflection.TypeInfo;
}
