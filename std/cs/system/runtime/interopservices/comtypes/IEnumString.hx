package cs.system.runtime.interopservices.comtypes;

/** Manages the definition of the  interface. */
@:native("System.Runtime.InteropServices.ComTypes.IEnumString")
extern interface IEnumString {
	/**
	 * Creates a new enumerator that contains the same enumeration state as the current
	 * one.
	 * @param ppenum When this method returns, contains a reference to the newly
	 * created enumerator. This parameter is passed uninitialized.
	 */
	function Clone(ppenum:cs.Ref<cs.system.runtime.interopservices.comtypes.IEnumString>):Void;
	/**
	 * Retrieves a specified number of items in the enumeration sequence.
	 * @param celt The number of strings to return in .
	 * @param rgelt When this method returns, contains a reference to the enumerated
	 * strings. This parameter is passed uninitialized.
	 * @param pceltFetched When this method returns, contains a reference to the actual
	 * number of strings enumerated in .
	 * @return if the  parameter equals the  parameter; otherwise, .
	 */
	function Next(celt:Int, rgelt:cs.NativeArray<String>, pceltFetched:cs.system.IntPtr):Int;
	/** Resets the enumeration sequence to the beginning. */
	function Reset():Void;
	/**
	 * Skips a specified number of items in the enumeration sequence.
	 * @param celt The number of elements to skip in the enumeration.
	 * @return if the number of elements skipped equals the  parameter; otherwise, .
	 */
	function Skip(celt:Int):Int;
}
