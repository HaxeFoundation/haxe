package cs.system.net.networkinformation;

/** Provides the Media Access Control (MAC) address for a network interface (adapter). */
@:native("System.Net.NetworkInformation.PhysicalAddress")
extern class PhysicalAddress {
	/** Returns a new  instance with a zero length address. This field is read-only. */
	static var None(default, never):cs.system.net.networkinformation.PhysicalAddress;
	function new(address:cs.NativeArray<cs.UInt8>):Void;
	/**
	 * Parses the specified  and stores its contents as the address bytes of the 
	 * returned by this method.
	 * @param address A  containing the address that will be used to initialize the 
	 * instance returned by this method.
	 * @return A  instance with the specified address.
	 */
	static function Parse(address:String):cs.system.net.networkinformation.PhysicalAddress;
	/**
	 * Compares two  instances.
	 * @param comparand The  to compare to the current instance.
	 * @return if this instance and the specified instance contain the same address;
	 * otherwise .
	 */
	function Equals(comparand:Dynamic):Bool;
	/**
	 * Returns the address of the current instance.
	 * @return A  array containing the address.
	 */
	function GetAddressBytes():cs.NativeArray<cs.UInt8>;
	/**
	 * Returns the hash value of a physical address.
	 * @return An integer hash value.
	 */
	function GetHashCode():Int;
	/**
	 * Returns the  representation of the address of this instance.
	 * @return A  containing the address contained in this instance.
	 */
	function ToString():String;
}
