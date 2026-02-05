package cs.system.collections.specialized;

/** Creates collections that ignore the case in strings. */
@:native("System.Collections.Specialized.CollectionsUtil")
extern class CollectionsUtil {
	function new():Void;
	@:overload(function():cs.system.collections.Hashtable {})
	@:overload(function(d:cs.system.collections.IDictionary):cs.system.collections.Hashtable {})
	/**
	 * Creates a new case-insensitive instance of the  class with the default initial
	 * capacity.
	 * @return A new case-insensitive instance of the  class with the default initial
	 * capacity.
	 */
	static function CreateCaseInsensitiveHashtable(capacity:Int):cs.system.collections.Hashtable;
	/**
	 * Creates a new instance of the  class that ignores the case of strings.
	 * @return A new instance of the  class that ignores the case of strings.
	 */
	static function CreateCaseInsensitiveSortedList():cs.system.collections.SortedList;
}
