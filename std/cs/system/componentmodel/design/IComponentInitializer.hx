package cs.system.componentmodel.design;

/** Provides a set of recommended default values during component creation. */
@:native("System.ComponentModel.Design.IComponentInitializer")
extern interface IComponentInitializer {
	/**
	 * Restores an instance of a component to its default state.
	 * @param defaultValues A dictionary of default property values, which are
	 * name/value pairs, with which to reset the component's state.
	 */
	function InitializeExistingComponent(defaultValues:cs.system.collections.IDictionary):Void;
	/**
	 * Initializes a new component using a set of recommended values.
	 * @param defaultValues A dictionary of default property values, which are
	 * name/value pairs, with which to initialize the component's state.
	 */
	function InitializeNewComponent(defaultValues:cs.system.collections.IDictionary):Void;
}
