package cs.system.componentmodel.design.serialization;

/** Provides a service that can generate unique names for objects. */
@:native("System.ComponentModel.Design.Serialization.INameCreationService")
extern interface INameCreationService {
	/**
	 * Creates a new name that is unique to all components in the specified container.
	 * @param container The container where the new object is added.
	 * @param dataType The data type of the object that receives the name.
	 * @return A unique name for the data type.
	 */
	function CreateName(container:cs.system.componentmodel.IContainer, dataType:cs.system.Type):String;
	/**
	 * Gets a value indicating whether the specified name is valid.
	 * @param name The name to validate.
	 * @return if the name is valid; otherwise, .
	 */
	function IsValidName(name:String):Bool;
	/**
	 * Gets a value indicating whether the specified name is valid.
	 * @param name The name to validate.
	 */
	function ValidateName(name:String):Void;
}
