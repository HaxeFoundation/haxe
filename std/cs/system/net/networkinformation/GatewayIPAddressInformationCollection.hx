package cs.system.net.networkinformation;

/** Stores a set of  types. */
@:native("System.Net.NetworkInformation.GatewayIPAddressInformationCollection")
extern class GatewayIPAddressInformationCollection {
	/**
	 * Gets the number of  types in this collection.
	 * @return An  value that contains the number of  types in this collection.
	 */
	var Count(default, never):Int;
	/**
	 * Gets a value that indicates whether access to this collection is read-only.
	 * @return in all cases.
	 */
	var IsReadOnly(default, never):Bool;
	@:native("get_Item")
	function get_Item(index0:Int):cs.system.net.networkinformation.GatewayIPAddressInformation;
	/**
	 * Throws a  because this operation is not supported for this collection.
	 * @param address The object to be added to the collection.
	 */
	function Add(address:cs.system.net.networkinformation.GatewayIPAddressInformation):Void;
	/** Throws a  because this operation is not supported for this collection. */
	function Clear():Void;
	/**
	 * Checks whether the collection contains the specified  object.
	 * @param address The  object to be searched in the collection.
	 * @return if the  object exists in the collection; otherwise .
	 */
	function Contains(address:cs.system.net.networkinformation.GatewayIPAddressInformation):Bool;
	/**
	 * Copies the elements in this collection to a one-dimensional array of type .
	 * @param array A one-dimensional array that receives a copy of the collection.
	 * @param offset The zero-based index in  at which the copy begins.
	 */
	function CopyTo(array:cs.NativeArray<cs.system.net.networkinformation.GatewayIPAddressInformation>, offset:Int):Void;
	/**
	 * Returns an object that can be used to iterate through this collection.
	 * @return An object that implements the  interface and provides access to the 
	 * types in this collection.
	 */
	function GetEnumerator():cs.system.collections.generic.IEnumerator<cs.system.net.networkinformation.GatewayIPAddressInformation>;
	/**
	 * Throws a  because this operation is not supported for this collection.
	 * @param address The object to be removed.
	 * @return Always throws a .
	 */
	function Remove(address:cs.system.net.networkinformation.GatewayIPAddressInformation):Bool;
}
