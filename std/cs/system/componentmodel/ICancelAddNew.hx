package cs.system.componentmodel;

/** Adds transactional capability when adding a new item to a collection. */
@:native("System.ComponentModel.ICancelAddNew")
extern interface ICancelAddNew {
	/**
	 * Discards a pending new item from the collection.
	 * @param itemIndex The index of the item that was previously added to the
	 * collection.
	 */
	function CancelNew(itemIndex:Int):Void;
	/**
	 * Commits a pending new item to the collection.
	 * @param itemIndex The index of the item that was previously added to the
	 * collection.
	 */
	function EndNew(itemIndex:Int):Void;
}
