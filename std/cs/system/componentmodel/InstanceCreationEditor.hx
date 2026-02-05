package cs.system.componentmodel;

/** Creates an instance of a particular type of property from a drop-down box within the . */
@:native("System.ComponentModel.InstanceCreationEditor")
extern class InstanceCreationEditor {
	/**
	 * Gets the specified text.
	 * @return The specified text.
	 */
	var Text(default, never):String;
	/**
	 * When overridden in a derived class, returns an instance of the specified type.
	 * @param context The context information.
	 * @param instanceType The specified type.
	 * @return An instance of the specified type or .
	 */
	function CreateInstance(context:cs.system.componentmodel.ITypeDescriptorContext, instanceType:cs.system.Type):Dynamic;
}
