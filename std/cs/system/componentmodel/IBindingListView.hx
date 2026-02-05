package cs.system.componentmodel;

/** Extends the  interface by providing advanced sorting and filtering capabilities. */
@:native("System.ComponentModel.IBindingListView")
extern interface IBindingListView extends cs.system.collections.ICollection extends cs.system.collections.IEnumerable extends cs.system.collections.IList extends cs.system.componentmodel.IBindingList {
	/**
	 * Gets or sets the filter to be used to exclude items from the collection of items
	 * returned by the data source
	 * @return The string used to filter items out in the item collection returned by
	 * the data source.
	 */
	var Filter(default, default):String;
	/**
	 * Gets the collection of sort descriptions currently applied to the data source.
	 * @return The  currently applied to the data source.
	 */
	var SortDescriptions(default, never):cs.system.componentmodel.ListSortDescriptionCollection;
	/**
	 * Gets a value indicating whether the data source supports advanced sorting.
	 * @return if the data source supports advanced sorting; otherwise, .
	 */
	var SupportsAdvancedSorting(default, never):Bool;
	/**
	 * Gets a value indicating whether the data source supports filtering.
	 * @return if the data source supports filtering; otherwise, .
	 */
	var SupportsFiltering(default, never):Bool;
	/**
	 * Sorts the data source based on the given .
	 * @param sorts The  containing the sorts to apply to the data source.
	 */
	function ApplySort(sorts:cs.system.componentmodel.ListSortDescriptionCollection):Void;
	/** Removes the current filter applied to the data source. */
	function RemoveFilter():Void;
}
