package cs.system.componentmodel.design;

/** Provides an interface for adding and removing extender providers at design time. */
@:native("System.ComponentModel.Design.IExtenderProviderService")
extern interface IExtenderProviderService {
	/**
	 * Adds the specified extender provider.
	 * @param provider The extender provider to add.
	 */
	function AddExtenderProvider(provider:cs.system.componentmodel.IExtenderProvider):Void;
	/**
	 * Removes the specified extender provider.
	 * @param provider The extender provider to remove.
	 */
	function RemoveExtenderProvider(provider:cs.system.componentmodel.IExtenderProvider):Void;
}
