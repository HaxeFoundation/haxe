package cs.system.net.sockets;

/** Presents the packet information from a call to  or . */
@:native("System.Net.Sockets.IPPacketInformation")
extern class IPPacketInformation extends cs.system.ValueType {
	/**
	 * Gets the origin information of the packet that was received as a result of
	 * calling the  method or  method.
	 * @return An  that indicates the origin information of the packet that was
	 * received as a result of calling the  method or  method. For packets that were
	 * sent from a unicast address, the  property will return the  of the sender; for
	 * multicast or broadcast packets, the  property will return the multicast or
	 * broadcast .
	 */
	var Address(default, never):cs.system.net.IPAddress;
	/**
	 * Gets the network interface information that is associated with a call to  or .
	 * @return An  value, which represents the index of the network interface. You can
	 * use this index with  to get more information about the relevant interface.
	 */
	var Interface(default, never):Int;
	/**
	 * Tests whether two specified  instances are equivalent.
	 * @param packetInformation1 The  instance that is to the left of the equality
	 * operator.
	 * @param packetInformation2 The  instance that is to the right of the equality
	 * operator.
	 * @return if  and  are equal; otherwise, .
	 */
	static function op_Equality(packetInformation1:cs.system.net.sockets.IPPacketInformation, packetInformation2:cs.system.net.sockets.IPPacketInformation):Bool;
	/**
	 * Tests whether two specified  instances are not equal.
	 * @param packetInformation1 The  instance that is to the left of the inequality
	 * operator.
	 * @param packetInformation2 The  instance that is to the right of the inequality
	 * operator.
	 * @return if  and  are unequal; otherwise, .
	 */
	static function op_Inequality(packetInformation1:cs.system.net.sockets.IPPacketInformation, packetInformation2:cs.system.net.sockets.IPPacketInformation):Bool;
	/**
	 * Returns a value that indicates whether this instance is equal to a specified
	 * object.
	 * @param comparand The object to compare with this instance.
	 * @return if  is an instance of  and equals the value of the instance; otherwise,
	 * .
	 */
	function Equals(comparand:Dynamic):Bool;
	/**
	 * Returns the hash code for this instance.
	 * @return An Int32 hash code.
	 */
	function GetHashCode():Int;
}
