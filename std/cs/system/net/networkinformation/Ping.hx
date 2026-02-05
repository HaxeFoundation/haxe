package cs.system.net.networkinformation;

/** Allows an application to determine whether a remote computer is accessible over the network. */
@:native("System.Net.NetworkInformation.Ping")
extern class Ping extends cs.system.componentmodel.Component {
	function new():Void;
	@:overload(function(address:cs.system.net.IPAddress):cs.system.net.networkinformation.PingReply {})
	@:overload(function(hostNameOrAddress:String):cs.system.net.networkinformation.PingReply {})
	@:overload(function(address:cs.system.net.IPAddress, timeout:Int):cs.system.net.networkinformation.PingReply {})
	@:overload(function(hostNameOrAddress:String, timeout:Int):cs.system.net.networkinformation.PingReply {})
	@:overload(function(address:cs.system.net.IPAddress, timeout:Int, buffer:cs.NativeArray<cs.UInt8>):cs.system.net.networkinformation.PingReply {})
	@:overload(function(hostNameOrAddress:String, timeout:Int, buffer:cs.NativeArray<cs.UInt8>):cs.system.net.networkinformation.PingReply {})
	@:overload(function(address:cs.system.net.IPAddress, timeout:Int, buffer:cs.NativeArray<cs.UInt8>, options:cs.system.net.networkinformation.PingOptions):cs.system.net.networkinformation.PingReply {})
	/**
	 * Attempts to send an Internet Control Message Protocol (ICMP) echo message to the
	 * computer that has the specified , and receive a corresponding ICMP echo reply
	 * message from that computer.
	 * @param address An  that identifies the computer that is the destination for the
	 * ICMP echo message.
	 * @return A  object that provides information about the ICMP echo reply message,
	 * if one was received, or describes the reason for the failure if no message was
	 * received.
	 */
	function Send(hostNameOrAddress:String, timeout:Int, buffer:cs.NativeArray<cs.UInt8>, options:cs.system.net.networkinformation.PingOptions):cs.system.net.networkinformation.PingReply;
	@:overload(function(address:cs.system.net.IPAddress, userToken:Dynamic):Void {})
	@:overload(function(hostNameOrAddress:String, userToken:Dynamic):Void {})
	@:overload(function(address:cs.system.net.IPAddress, timeout:Int, userToken:Dynamic):Void {})
	@:overload(function(hostNameOrAddress:String, timeout:Int, userToken:Dynamic):Void {})
	@:overload(function(address:cs.system.net.IPAddress, timeout:Int, buffer:cs.NativeArray<cs.UInt8>, userToken:Dynamic):Void {})
	@:overload(function(hostNameOrAddress:String, timeout:Int, buffer:cs.NativeArray<cs.UInt8>, userToken:Dynamic):Void {})
	@:overload(function(address:cs.system.net.IPAddress, timeout:Int, buffer:cs.NativeArray<cs.UInt8>, options:cs.system.net.networkinformation.PingOptions, userToken:Dynamic):Void {})
	/**
	 * Asynchronously attempts to send an Internet Control Message Protocol (ICMP) echo
	 * message with the specified data buffer to the computer that has the specified ,
	 * and receive a corresponding ICMP echo reply message from that computer. This
	 * overload allows you to specify a time-out value for the operation and control
	 * fragmentation and Time-to-Live values for the ICMP echo message packet.
	 * @param address An  that identifies the computer that is the destination for the
	 * ICMP echo message.
	 * @param timeout An  value that specifies the maximum number of milliseconds
	 * (after sending the echo message) to wait for the ICMP echo reply message.
	 * @param buffer A  array that contains data to be sent with the ICMP echo message
	 * and returned in the ICMP echo reply message. The array cannot contain more than
	 * 65,500 bytes.
	 * @param options A  object used to control fragmentation and Time-to-Live values
	 * for the ICMP echo message packet.
	 * @param userToken An object that is passed to the method invoked when the
	 * asynchronous operation completes.
	 */
	function SendAsync(hostNameOrAddress:String, timeout:Int, buffer:cs.NativeArray<cs.UInt8>, options:cs.system.net.networkinformation.PingOptions, userToken:Dynamic):Void;
	/** Cancels all pending asynchronous requests to send an Internet Control Message Protocol (ICMP) echo message and receives a corresponding ICMP echo reply message. */
	function SendAsyncCancel():Void;
	@:overload(function(address:cs.system.net.IPAddress):cs.system.threading.tasks.Task_1<cs.system.net.networkinformation.PingReply> {})
	@:overload(function(hostNameOrAddress:String):cs.system.threading.tasks.Task_1<cs.system.net.networkinformation.PingReply> {})
	@:overload(function(address:cs.system.net.IPAddress, timeout:Int):cs.system.threading.tasks.Task_1<cs.system.net.networkinformation.PingReply> {})
	@:overload(function(hostNameOrAddress:String, timeout:Int):cs.system.threading.tasks.Task_1<cs.system.net.networkinformation.PingReply> {})
	@:overload(function(address:cs.system.net.IPAddress, timeout:Int, buffer:cs.NativeArray<cs.UInt8>):cs.system.threading.tasks.Task_1<cs.system.net.networkinformation.PingReply> {})
	@:overload(function(hostNameOrAddress:String, timeout:Int, buffer:cs.NativeArray<cs.UInt8>):cs.system.threading.tasks.Task_1<cs.system.net.networkinformation.PingReply> {})
	@:overload(function(address:cs.system.net.IPAddress, timeout:Int, buffer:cs.NativeArray<cs.UInt8>, options:cs.system.net.networkinformation.PingOptions):cs.system.threading.tasks.Task_1<cs.system.net.networkinformation.PingReply> {})
	/**
	 * Send an Internet Control Message Protocol (ICMP) echo message with the specified
	 * data buffer to the computer that has the specified , and receives a
	 * corresponding ICMP echo reply message from that computer as an asynchronous
	 * operation.
	 * @param address An IP address that identifies the computer that is the
	 * destination for the ICMP echo message.
	 * @return The task object representing the asynchronous operation.
	 */
	function SendPingAsync(hostNameOrAddress:String, timeout:Int, buffer:cs.NativeArray<cs.UInt8>, options:cs.system.net.networkinformation.PingOptions):cs.system.threading.tasks.Task_1<cs.system.net.networkinformation.PingReply>;
}
