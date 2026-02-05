package cs.system.componentmodel.design;

/** Provides an interface that can list extender providers. */
@:native("System.ComponentModel.Design.IExtenderListService")
extern interface IExtenderListService {
	/**
	 * Gets the set of extender providers for the component.
	 * @return An array of type  that lists the active extender providers. If there are
	 * no providers, an empty array is returned.
	 */
	function GetExtenderProviders():cs.NativeArray<cs.system.componentmodel.IExtenderProvider>;
}
