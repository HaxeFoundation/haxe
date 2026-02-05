package cs.system.runtime.interopservices.comtypes;

/** Provides the managed definition of the  interface. */
@:native("System.Runtime.InteropServices.ComTypes.IEnumSTATDATA")
extern interface IEnumSTATDATA {
	/**
	 * Creates a new enumerator that contains the same enumeration state as the current
	 * enumerator.
	 * @param newEnum When this method returns, contains a reference to the newly
	 * created enumerator. This parameter is passed uninitialized.
	 */
	function Clone(newEnum:cs.Ref<cs.system.runtime.interopservices.comtypes.IEnumSTATDATA>):Void;
	/**
	 * Retrieves a specified number of items in the enumeration sequence.
	 * @param celt The number of  references to return in .
	 * @param rgelt When this method returns, contains a reference to the enumerated 
	 * references. This parameter is passed uninitialized.
	 * @param pceltFetched When this parameter returns, contains a reference to the
	 * actual number of references enumerated in . This parameter is passed
	 * uninitialized.
	 * @return if the  parameter equals the  parameter; otherwise, .
	 */
	function Next(celt:Int, rgelt:cs.NativeArray<cs.system.runtime.interopservices.comtypes.STATDATA>, pceltFetched:cs.NativeArray<Int>):Int;
	/**
	 * Resets the enumeration sequence to the beginning.
	 * @return An HRESULT with the value .
	 */
	function Reset():Int;
	/**
	 * Skips a specified number of items in the enumeration sequence.
	 * @param celt The number of elements to skip in the enumeration.
	 * @return if the number of elements skipped equals the  parameter; otherwise, .
	 */
	function Skip(celt:Int):Int;
}
