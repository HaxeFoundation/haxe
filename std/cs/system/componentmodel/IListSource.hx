package cs.system.componentmodel;

/** Provides functionality to an object to return a list that can be bound to a data source. */
@:native("System.ComponentModel.IListSource")
extern interface IListSource {
	/**
	 * Gets a value indicating whether the collection is a collection of  objects.
	 * @return if the collection is a collection of  objects; otherwise, .
	 */
	var ContainsListCollection(default, never):Bool;
	/**
	 * Returns an  that can be bound to a data source from an object that does not
	 * implement an  itself.
	 * @return An  that can be bound to a data source from the object.
	 */
	function GetList():cs.system.collections.IList;
}
