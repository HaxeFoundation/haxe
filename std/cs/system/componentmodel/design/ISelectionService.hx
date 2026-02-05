package cs.system.componentmodel.design;

/** Provides an interface for a designer to select components. */
@:native("System.ComponentModel.Design.ISelectionService")
extern interface ISelectionService {
	/**
	 * Gets the object that is currently the primary selected object.
	 * @return The object that is currently the primary selected object.
	 */
	var PrimarySelection(default, never):Dynamic;
	/**
	 * Gets the count of selected objects.
	 * @return The number of selected objects.
	 */
	var SelectionCount(default, never):Int;
	/**
	 * Gets a value indicating whether the specified component is currently selected.
	 * @param component The component to test.
	 * @return if the component is part of the user's current selection; otherwise, .
	 */
	function GetComponentSelected(component:Dynamic):Bool;
	/**
	 * Gets a collection of components that are currently selected.
	 * @return A collection that represents the current set of components that are
	 * selected.
	 */
	function GetSelectedComponents():cs.system.collections.ICollection;
	@:overload(function(components:cs.system.collections.ICollection):Void {})
	/**
	 * Selects the specified collection of components.
	 * @param components The collection of components to select.
	 */
	function SetSelectedComponents(components:cs.system.collections.ICollection, selectionType:cs.system.componentmodel.design.SelectionTypes):Void;
}
