package cs.system.componentmodel.design;

/** Discovers available types at design time. */
@:native("System.ComponentModel.Design.ITypeDiscoveryService")
extern interface ITypeDiscoveryService {
	/**
	 * Retrieves the list of available types.
	 * @param baseType The base type to match. Can be .
	 * @param excludeGlobalTypes Indicates whether types from all referenced assemblies
	 * should be checked.
	 * @return A collection of types that match the criteria specified by  and .
	 */
	function GetTypes(baseType:cs.system.Type, excludeGlobalTypes:Bool):cs.system.collections.ICollection;
}
