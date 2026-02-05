package cs.system.net.sockets;

/** Presents UDP receive result information from a call to the  method. */
@:native("System.Net.Sockets.UdpReceiveResult")
extern class UdpReceiveResult extends cs.system.ValueType {
	/**
	 * Gets a buffer with the data received in the UDP packet.
	 * @return A  array with the data received in the UDP packet.
	 */
	var Buffer(default, never):cs.NativeArray<cs.UInt8>;
	/**
	 * Gets the remote endpoint from which the UDP packet was received.
	 * @return The remote endpoint from which the UDP packet was received.
	 */
	var RemoteEndPoint(default, never):cs.system.net.IPEndPoint;
	function new(buffer:cs.NativeArray<cs.UInt8>, remoteEndPoint:cs.system.net.IPEndPoint):Void;
	/**
	 * Tests whether two specified  instances are equivalent.
	 * @param left The  instance that is to the left of the equality operator.
	 * @param right The  instance that is to the right of the equality operator.
	 * @return if  and  are equal; otherwise, .
	 */
	static function op_Equality(left:cs.system.net.sockets.UdpReceiveResult, right:cs.system.net.sockets.UdpReceiveResult):Bool;
	/**
	 * Tests whether two specified  instances are not equal.
	 * @param left The  instance that is to the left of the not equal operator.
	 * @param right The  instance that is to the right of the not equal operator.
	 * @return if  and  are unequal; otherwise, .
	 */
	static function op_Inequality(left:cs.system.net.sockets.UdpReceiveResult, right:cs.system.net.sockets.UdpReceiveResult):Bool;
	@:overload(function(other:cs.system.net.sockets.UdpReceiveResult):Bool {})
	/**
	 * Returns a value that indicates whether this instance is equal to a specified
	 * object.
	 * @param other The object to compare with this instance.
	 * @return if  is an instance of  and equals the value of the instance; otherwise,
	 * .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Returns the hash code for this instance.
	 * @return The hash code.
	 */
	function GetHashCode():Int;
}
