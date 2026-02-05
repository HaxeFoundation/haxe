package cs.system.componentmodel;

/** Provides functionality to discover the schema for a bindable list, where the properties available for binding differ from the public properties of the object to bind to. */
@:native("System.ComponentModel.ITypedList")
extern interface ITypedList {
	/**
	 * Returns the  that represents the properties on each item used to bind data.
	 * @param listAccessors An array of  objects to find in the collection as bindable.
	 * This can be .
	 * @return The  that represents the properties on each item used to bind data.
	 */
	function GetItemProperties(listAccessors:cs.NativeArray<cs.system.componentmodel.PropertyDescriptor>):cs.system.componentmodel.PropertyDescriptorCollection;
	/**
	 * Returns the name of the list.
	 * @param listAccessors An array of  objects, for which the list name is returned.
	 * This can be .
	 * @return The name of the list.
	 */
	function GetListName(listAccessors:cs.NativeArray<cs.system.componentmodel.PropertyDescriptor>):String;
}
