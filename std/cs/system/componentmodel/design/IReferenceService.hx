package cs.system.componentmodel.design;

/** Provides an interface for obtaining references to objects within a project by name or type, obtaining the name of a specified object, and for locating the parent of a specified object within a designer project. */
@:native("System.ComponentModel.Design.IReferenceService")
extern interface IReferenceService {
	/**
	 * Gets the component that contains the specified component.
	 * @param reference The object to retrieve the parent component for.
	 * @return The base  that contains the specified object, or  if no parent component
	 * exists.
	 */
	function GetComponent(reference:Dynamic):cs.system.componentmodel.IComponent;
	/**
	 * Gets the name of the specified component.
	 * @param reference The object to return the name of.
	 * @return The name of the object referenced, or  if the object reference is not
	 * valid.
	 */
	function GetName(reference:Dynamic):String;
	/**
	 * Gets a reference to the component whose name matches the specified name.
	 * @param name The name of the component to return a reference to.
	 * @return An object the specified name refers to, or  if no reference is found.
	 */
	function GetReference(name:String):Dynamic;
	@:overload(function():cs.NativeArray<Dynamic> {})
	/**
	 * Gets all available references to project components.
	 * @return An array of all objects with references available to the .
	 */
	function GetReferences(baseType:cs.system.Type):cs.NativeArray<Dynamic>;
}
