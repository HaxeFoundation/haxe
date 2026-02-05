package cs.system.componentmodel.design;

/** Enables enumeration of components at design time. */
@:native("System.ComponentModel.Design.IComponentDiscoveryService")
extern interface IComponentDiscoveryService {
	/**
	 * Gets the list of available component types.
	 * @param designerHost The designer host providing design-time services. Can be .
	 * @param baseType The base type specifying the components to retrieve. Can be .
	 * @return The list of available component types.
	 */
	function GetComponentTypes(designerHost:cs.system.componentmodel.design.IDesignerHost, baseType:cs.system.Type):cs.system.collections.ICollection;
}
