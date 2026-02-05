package cs.system.componentmodel;

/** Provides a description of the sort operation applied to a data source. */
@:native("System.ComponentModel.ListSortDescription")
extern class ListSortDescription {
	/**
	 * Gets or sets the abstract description of a class property associated with this
	 * @return The  associated with this .
	 */
	var PropertyDescriptor(default, default):cs.system.componentmodel.PropertyDescriptor;
	/**
	 * Gets or sets the direction of the sort operation associated with this .
	 * @return One of the  values.
	 */
	var SortDirection(default, default):cs.system.componentmodel.ListSortDirection;
	function new(property:cs.system.componentmodel.PropertyDescriptor, direction:cs.system.componentmodel.ListSortDirection):Void;
}
