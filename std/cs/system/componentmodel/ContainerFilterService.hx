package cs.system.componentmodel;

/** Provides a base class for the container filter service. */
@:native("System.ComponentModel.ContainerFilterService")
extern class ContainerFilterService {
	/**
	 * Filters the component collection.
	 * @param components The component collection to filter.
	 * @return A  that represents a modified collection.
	 */
	function FilterComponents(components:cs.system.componentmodel.ComponentCollection):cs.system.componentmodel.ComponentCollection;
}
