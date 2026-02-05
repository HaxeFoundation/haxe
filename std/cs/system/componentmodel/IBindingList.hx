package cs.system.componentmodel;

/** Provides the features required to support both complex and simple scenarios when binding to a data source. */
@:native("System.ComponentModel.IBindingList")
extern interface IBindingList extends cs.system.collections.ICollection extends cs.system.collections.IEnumerable extends cs.system.collections.IList {
	/**
	 * Gets whether you can update items in the list.
	 * @return if you can update the items in the list; otherwise, .
	 */
	var AllowEdit(default, never):Bool;
	/**
	 * Gets whether you can add items to the list using .
	 * @return if you can add items to the list using ; otherwise, .
	 */
	var AllowNew(default, never):Bool;
	/**
	 * Gets whether you can remove items from the list, using  or .
	 * @return if you can remove items from the list; otherwise, .
	 */
	var AllowRemove(default, never):Bool;
	/**
	 * Gets whether the items in the list are sorted.
	 * @return if  has been called and  has not been called; otherwise, .
	 */
	var IsSorted(default, never):Bool;
	/**
	 * Gets the direction of the sort.
	 * @return One of the  values.
	 */
	var SortDirection(default, never):cs.system.componentmodel.ListSortDirection;
	/**
	 * Gets the  that is being used for sorting.
	 * @return The  that is being used for sorting.
	 */
	var SortProperty(default, never):cs.system.componentmodel.PropertyDescriptor;
	/**
	 * Gets whether a  event is raised when the list changes or an item in the list
	 * changes.
	 * @return if a  event is raised when the list changes or when an item changes;
	 * otherwise, .
	 */
	var SupportsChangeNotification(default, never):Bool;
	/**
	 * Gets whether the list supports searching using the  method.
	 * @return if the list supports searching using the  method; otherwise, .
	 */
	var SupportsSearching(default, never):Bool;
	/**
	 * Gets whether the list supports sorting.
	 * @return if the list supports sorting; otherwise, .
	 */
	var SupportsSorting(default, never):Bool;
	/**
	 * Adds the  to the indexes used for searching.
	 * @param property The  to add to the indexes used for searching.
	 */
	function AddIndex(property:cs.system.componentmodel.PropertyDescriptor):Void;
	/**
	 * Adds a new item to the list.
	 * @return The item added to the list.
	 */
	function AddNew():Dynamic;
	/**
	 * Sorts the list based on a  and a .
	 * @param property The  to sort by.
	 * @param direction One of the  values.
	 */
	function ApplySort(property:cs.system.componentmodel.PropertyDescriptor, direction:cs.system.componentmodel.ListSortDirection):Void;
	/**
	 * Returns the index of the row that has the given .
	 * @param property The  to search on.
	 * @param key The value of the  parameter to search for.
	 * @return The index of the row that has the given .
	 */
	function Find(property:cs.system.componentmodel.PropertyDescriptor, key:Dynamic):Int;
	/**
	 * Removes the  from the indexes used for searching.
	 * @param property The  to remove from the indexes used for searching.
	 */
	function RemoveIndex(property:cs.system.componentmodel.PropertyDescriptor):Void;
	/** Removes any sort applied using . */
	function RemoveSort():Void;
}
