package cs.system.componentmodel;

/** Provides the base class for a custom component editor. */
@:native("System.ComponentModel.ComponentEditor")
extern class ComponentEditor {
	@:overload(function(component:Dynamic):Bool {})
	/**
	 * Edits the component and returns a value indicating whether the component was
	 * modified based upon a given context.
	 * @param context An optional context object that can be used to obtain further
	 * information about the edit.
	 * @param component The component to be edited.
	 * @return if the component was modified; otherwise, .
	 */
	function EditComponent(context:cs.system.componentmodel.ITypeDescriptorContext, component:Dynamic):Bool;
}
