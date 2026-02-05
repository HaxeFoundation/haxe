package cs.system.componentmodel.design;

/** Provides the basic framework for building a custom designer. */
@:native("System.ComponentModel.Design.IDesigner")
extern interface IDesigner extends cs.system.IDisposable {
	/**
	 * Gets the base component that this designer is designing.
	 * @return An  indicating the base component that this designer is designing.
	 */
	var Component(default, never):cs.system.componentmodel.IComponent;
	/**
	 * Gets a collection of the design-time verbs supported by the designer.
	 * @return A  that contains the verbs supported by the designer, or  if the
	 * component has no verbs.
	 */
	var Verbs(default, never):cs.system.componentmodel.design.DesignerVerbCollection;
	/** Performs the default action for this designer. */
	function DoDefaultAction():Void;
	/**
	 * Initializes the designer with the specified component.
	 * @param component The component to associate with this designer.
	 */
	function Initialize(component:cs.system.componentmodel.IComponent):Void;
}
