package cs.system.reflection.emit;

/** Defines and represents a dynamic assembly. */
@:native("System.Reflection.Emit.AssemblyBuilder")
extern class AssemblyBuilder extends cs.system.reflection.Assembly {
	@:overload(function(name:cs.system.reflection.AssemblyName, access:cs.system.reflection.emit.AssemblyBuilderAccess):cs.system.reflection.emit.AssemblyBuilder {})
	/**
	 * Defines a dynamic assembly that has the specified name and access rights.
	 * @param name The name of the assembly.
	 * @param access The access rights of the assembly.
	 * @return An object that represents the new assembly.
	 */
	static function DefineDynamicAssembly(name:cs.system.reflection.AssemblyName, access:cs.system.reflection.emit.AssemblyBuilderAccess, assemblyAttributes:cs.system.collections.generic.IEnumerable<cs.system.reflection.emit.CustomAttributeBuilder>):cs.system.reflection.emit.AssemblyBuilder;
	/**
	 * Defines a named transient dynamic module in this assembly.
	 * @param name The name of the dynamic module.
	 * @return A  representing the defined dynamic module.
	 */
	function DefineDynamicModule(name:String):cs.system.reflection.emit.ModuleBuilder;
	/**
	 * Returns a value that indicates whether this instance is equal to the specified
	 * object.
	 * @param obj An object to compare with this instance, or .
	 * @return if  equals the type and value of this instance; otherwise, .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Returns the dynamic module with the specified name.
	 * @param name The name of the requested dynamic module.
	 * @return A ModuleBuilder object representing the requested dynamic module.
	 */
	function GetDynamicModule(name:String):cs.system.reflection.emit.ModuleBuilder;
	/**
	 * Returns the hash code for this instance.
	 * @return A 32-bit signed integer hash code.
	 */
	function GetHashCode():Int;
	/**
	 * Returns information about how the given resource has been persisted.
	 * @param resourceName The name of the resource.
	 * @return populated with information about the resource's topology, or  if the
	 * resource is not found.
	 */
	function GetManifestResourceInfo(resourceName:String):cs.system.reflection.ManifestResourceInfo;
	/**
	 * Loads the specified manifest resource from this assembly.
	 * @return An array of type  containing the names of all the resources.
	 */
	function GetManifestResourceNames():cs.NativeArray<String>;
	/**
	 * Loads the specified manifest resource from this assembly.
	 * @param name The name of the manifest resource being requested.
	 * @return A  representing this manifest resource.
	 */
	function GetManifestResourceStream(name:String):cs.system.io.Stream;
	@:overload(function(customBuilder:cs.system.reflection.emit.CustomAttributeBuilder):Void {})
	/**
	 * Set a custom attribute on this assembly using a specified custom attribute blob.
	 * @param con The constructor for the custom attribute.
	 * @param binaryAttribute A byte blob representing the attributes.
	 */
	function SetCustomAttribute(con:cs.system.reflection.ConstructorInfo, binaryAttribute:cs.NativeArray<cs.UInt8>):Void;
}
