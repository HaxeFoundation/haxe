package cs.system.runtime.interopservices.comtypes;

/** Manages the definition of the  interface. */
@:native("System.Runtime.InteropServices.ComTypes.IEnumVARIANT")
extern interface IEnumVARIANT {
	/**
	 * Creates a new enumerator that contains the same enumeration state as the current
	 * one.
	 * @return An  reference to the newly created enumerator.
	 */
	function Clone():cs.system.runtime.interopservices.comtypes.IEnumVARIANT;
	/**
	 * Retrieves a specified number of items in the enumeration sequence.
	 * @param celt The number of elements to return in rgelt.
	 * @param rgVar When this method returns, contains a reference to the enumerated
	 * elements. This parameter is passed uninitialized.
	 * @param pceltFetched When this method returns, contains a reference to the actual
	 * number of elements enumerated in rgelt.
	 * @return if the  parameter equals the  parameter; otherwise, .
	 */
	function Next(celt:Int, rgVar:cs.NativeArray<Dynamic>, pceltFetched:cs.system.IntPtr):Int;
	/**
	 * Resets the enumeration sequence to the beginning.
	 * @return An HRESULT with the value .
	 */
	function Reset():Int;
	/**
	 * Skips a specified number of items in the enumeration sequence.
	 * @param celt The number of elements to skip in the enumeration.
	 * @return if the number of elements skipped equals  parameter; otherwise, .
	 */
	function Skip(celt:Int):Int;
}
