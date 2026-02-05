package cs.system.net;

/** Provides an Internet Protocol (IP) address. */
@:native("System.Net.IPAddress")
extern class IPAddress {
	/** Provides an IP address that indicates that the server must listen for client activity on all network interfaces. This field is read-only. */
	static var Any(default, never):cs.system.net.IPAddress;
	/** Provides the IP broadcast address. This field is read-only. */
	static var Broadcast(default, never):cs.system.net.IPAddress;
	/** The  method uses the  field to indicate that a  must listen for client activity on all network interfaces. */
	static var IPv6Any(default, never):cs.system.net.IPAddress;
	/** Provides the IP loopback address. This property is read-only. */
	static var IPv6Loopback(default, never):cs.system.net.IPAddress;
	/** Provides an IP address that indicates that no network interface should be used. This property is read-only. */
	static var IPv6None(default, never):cs.system.net.IPAddress;
	/** Provides the IP loopback address. This field is read-only. */
	static var Loopback(default, never):cs.system.net.IPAddress;
	/** Provides an IP address that indicates that no network interface should be used. This field is read-only. */
	static var None(default, never):cs.system.net.IPAddress;
	/**
	 * An Internet Protocol (IP) address.
	 * @return The long value of the IP address.
	 */
	var Address(default, default):haxe.Int64;
	/**
	 * Gets the address family of the IP address.
	 * @return Returns  for IPv4 or  for IPv6.
	 */
	var AddressFamily(default, never):cs.system.net.sockets.AddressFamily;
	/**
	 * Gets whether the IP address is an IPv4-mapped IPv6 address.
	 * @return Returns . if the IP address is an IPv4-mapped IPv6 address; otherwise, .
	 */
	var IsIPv4MappedToIPv6(default, never):Bool;
	/**
	 * Gets whether the address is an IPv6 link local address.
	 * @return if the IP address is an IPv6 link local address; otherwise, .
	 */
	var IsIPv6LinkLocal(default, never):Bool;
	/**
	 * Gets whether the address is an IPv6 multicast global address.
	 * @return if the IP address is an IPv6 multicast global address; otherwise, .
	 */
	var IsIPv6Multicast(default, never):Bool;
	/**
	 * Gets whether the address is an IPv6 site local address.
	 * @return if the IP address is an IPv6 site local address; otherwise, .
	 */
	var IsIPv6SiteLocal(default, never):Bool;
	/**
	 * Gets whether the address is an IPv6 Teredo address.
	 * @return if the IP address is an IPv6 Teredo address; otherwise, .
	 */
	var IsIPv6Teredo(default, never):Bool;
	/**
	 * Gets or sets the IPv6 address scope identifier.
	 * @return A long integer that specifies the scope of the address.
	 */
	var ScopeId(default, default):haxe.Int64;
	@:overload(function(address:cs.NativeArray<cs.UInt8>):Void {})
	@:overload(function(newAddress:haxe.Int64):Void {})
	@:overload(function(address:cs.system.ReadOnlySpan<cs.UInt8>):Void {})
	@:overload(function(address:cs.NativeArray<cs.UInt8>, scopeid:haxe.Int64):Void {})
	function new(address:cs.system.ReadOnlySpan<cs.UInt8>, scopeid:haxe.Int64):Void;
	@:overload(function(host:cs.Int16):cs.Int16 {})
	@:overload(function(host:Int):Int {})
	/**
	 * Converts a short value from host byte order to network byte order.
	 * @param host The number to convert, expressed in host byte order.
	 * @return A short value, expressed in network byte order.
	 */
	static function HostToNetworkOrder(host:haxe.Int64):haxe.Int64;
	/**
	 * Indicates whether the specified IP address is the loopback address.
	 * @param address An IP address.
	 * @return if  is the loopback address; otherwise, .
	 */
	static function IsLoopback(address:cs.system.net.IPAddress):Bool;
	@:overload(function(network:cs.Int16):cs.Int16 {})
	@:overload(function(network:Int):Int {})
	/**
	 * Converts a short value from network byte order to host byte order.
	 * @param network The number to convert, expressed in network byte order.
	 * @return A short value, expressed in host byte order.
	 */
	static function NetworkToHostOrder(network:haxe.Int64):haxe.Int64;
	@:overload(function(ipString:cs.system.ReadOnlySpan<cs.Char16>):cs.system.net.IPAddress {})
	/** @param ipString  */
	static function Parse(ipString:String):cs.system.net.IPAddress;
	@:overload(function(ipString:cs.system.ReadOnlySpan<cs.Char16>, address:cs.Ref<cs.system.net.IPAddress>):Bool {})
	/**
	 * @param ipString 
	 * @param address 
	 */
	static function TryParse(ipString:String, address:cs.Ref<cs.system.net.IPAddress>):Bool;
	/**
	 * Compares two IP addresses.
	 * @param comparand An  instance to compare to the current instance.
	 * @return if the two addresses are equal; otherwise, .
	 */
	function Equals(comparand:Dynamic):Bool;
	/**
	 * Provides a copy of the  as an array of bytes.
	 * @return A  array.
	 */
	function GetAddressBytes():cs.NativeArray<cs.UInt8>;
	/**
	 * Returns a hash value for an IP address.
	 * @return An integer hash value.
	 */
	function GetHashCode():Int;
	/**
	 * Maps the  object to an IPv4 address.
	 * @return Returns . An IPv4 address.
	 */
	function MapToIPv4():cs.system.net.IPAddress;
	/**
	 * Maps the  object to an IPv6 address.
	 * @return Returns . An IPv6 address.
	 */
	function MapToIPv6():cs.system.net.IPAddress;
	/**
	 * Converts an Internet address to its standard notation.
	 * @return A string that contains the IP address in either IPv4 dotted-quad or in
	 * IPv6 colon-hexadecimal notation.
	 */
	function ToString():String;
	/**
	 * @param destination 
	 * @param charsWritten 
	 */
	function TryFormat(destination:cs.system.Span<cs.Char16>, charsWritten:cs.Ref<Int>):Bool;
	/**
	 * @param destination 
	 * @param bytesWritten 
	 */
	function TryWriteBytes(destination:cs.system.Span<cs.UInt8>, bytesWritten:cs.Ref<Int>):Bool;
}
