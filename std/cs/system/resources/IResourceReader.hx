package cs.system.resources;

/** Provides the base functionality for reading data from resource files. */
@:native("System.Resources.IResourceReader")
extern interface IResourceReader extends cs.system.collections.IEnumerable extends cs.system.IDisposable {
	/** Closes the resource reader after releasing any resources associated with it. */
	function Close():Void;
	/**
	 * Returns a dictionary enumerator of the resources for this reader.
	 * @return A dictionary enumerator for the resources for this reader.
	 */
	function GetEnumerator():cs.system.collections.IDictionaryEnumerator;
}
