package cs.system.componentmodel.design;

/** Provides a type description provider for a specified type. */
@:native("System.ComponentModel.Design.TypeDescriptionProviderService")
extern class TypeDescriptionProviderService {
	@:overload(function(instance:Dynamic):cs.system.componentmodel.TypeDescriptionProvider {})
	/**
	 * Gets a type description provider for the specified object.
	 * @param instance The object to get a type description provider for.
	 * @return A  that corresponds with .
	 */
	function GetProvider(type:cs.system.Type):cs.system.componentmodel.TypeDescriptionProvider;
}
