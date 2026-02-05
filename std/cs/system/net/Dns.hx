package cs.system.net;

/** Provides simple domain name resolution functionality. */
@:native("System.Net.Dns")
extern class Dns {
	/**
	 * Asynchronously returns the Internet Protocol (IP) addresses for the specified
	 * host.
	 * @param hostNameOrAddress The host name or IP address to resolve.
	 * @param requestCallback An  delegate that references the method to invoke when
	 * the operation is complete.
	 * @param state A user-defined object that contains information about the
	 * operation. This object is passed to the  delegate when the operation is
	 * complete.
	 * @return An  instance that references the asynchronous request.
	 */
	static function BeginGetHostAddresses(hostNameOrAddress:String, requestCallback:cs.system.AsyncCallback, state:Dynamic):cs.system.IAsyncResult;
	/**
	 * Begins an asynchronous request for  information about the specified DNS host
	 * name.
	 * @param hostName The DNS name of the host.
	 * @param requestCallback An  delegate that references the method to invoke when
	 * the operation is complete.
	 * @param stateObject A user-defined object that contains information about the
	 * operation. This object is passed to the  delegate when the operation is
	 * complete.
	 * @return An  instance that references the asynchronous request.
	 */
	static function BeginGetHostByName(hostName:String, requestCallback:cs.system.AsyncCallback, stateObject:Dynamic):cs.system.IAsyncResult;
	@:overload(function(address:cs.system.net.IPAddress, requestCallback:cs.system.AsyncCallback, stateObject:Dynamic):cs.system.IAsyncResult {})
	/**
	 * Asynchronously resolves an IP address to an  instance.
	 * @param address The IP address to resolve.
	 * @param requestCallback An  delegate that references the method to invoke when
	 * the operation is complete.
	 * @param stateObject A user-defined object that contains information about the
	 * operation. This object is passed to the  delegate when the operation is
	 * complete.
	 * @return An  instance that references the asynchronous request.
	 */
	static function BeginGetHostEntry(hostNameOrAddress:String, requestCallback:cs.system.AsyncCallback, stateObject:Dynamic):cs.system.IAsyncResult;
	/**
	 * Begins an asynchronous request to resolve a DNS host name or IP address to an 
	 * instance.
	 * @param hostName The DNS name of the host.
	 * @param requestCallback An  delegate that references the method to invoke when
	 * the operation is complete.
	 * @param stateObject A user-defined object that contains information about the
	 * operation. This object is passed to the  delegate when the operation is
	 * complete.
	 * @return An  instance that references the asynchronous request.
	 */
	static function BeginResolve(hostName:String, requestCallback:cs.system.AsyncCallback, stateObject:Dynamic):cs.system.IAsyncResult;
	/**
	 * Ends an asynchronous request for DNS information.
	 * @param asyncResult An  instance returned by a call to the  method.
	 * @return An array of type  that holds the IP addresses for the host specified by
	 * the  parameter of .
	 */
	static function EndGetHostAddresses(asyncResult:cs.system.IAsyncResult):cs.NativeArray<cs.system.net.IPAddress>;
	/**
	 * Ends an asynchronous request for DNS information.
	 * @param asyncResult An  instance that is returned by a call to the  method.
	 * @return An  object that contains DNS information about a host.
	 */
	static function EndGetHostByName(asyncResult:cs.system.IAsyncResult):cs.system.net.IPHostEntry;
	/**
	 * Ends an asynchronous request for DNS information.
	 * @param asyncResult An  instance returned by a call to an  method.
	 * @return An  instance that contains address information about the host.
	 */
	static function EndGetHostEntry(asyncResult:cs.system.IAsyncResult):cs.system.net.IPHostEntry;
	/**
	 * Ends an asynchronous request for DNS information.
	 * @param asyncResult An  instance that is returned by a call to the  method.
	 * @return An  object that contains DNS information about a host.
	 */
	static function EndResolve(asyncResult:cs.system.IAsyncResult):cs.system.net.IPHostEntry;
	/**
	 * Returns the Internet Protocol (IP) addresses for the specified host.
	 * @param hostNameOrAddress The host name or IP address to resolve.
	 * @return An array of type  that holds the IP addresses for the host that is
	 * specified by the  parameter.
	 */
	static function GetHostAddresses(hostNameOrAddress:String):cs.NativeArray<cs.system.net.IPAddress>;
	/**
	 * Returns the Internet Protocol (IP) addresses for the specified host as an
	 * asynchronous operation.
	 * @param hostNameOrAddress The host name or IP address to resolve.
	 * @return The task object representing the asynchronous operation. The  property
	 * on the task object returns an array of type  that holds the IP addresses for the
	 * host that is specified by the  parameter.
	 */
	static function GetHostAddressesAsync(hostNameOrAddress:String):cs.system.threading.tasks.Task_1<cs.NativeArray<cs.system.net.IPAddress>>;
	@:overload(function(address:cs.system.net.IPAddress):cs.system.net.IPHostEntry {})
	/**
	 * Creates an  instance from the specified .
	 * @param address An .
	 * @return An  instance.
	 */
	static function GetHostByAddress(address:String):cs.system.net.IPHostEntry;
	/**
	 * Gets the DNS information for the specified DNS host name.
	 * @param hostName The DNS name of the host.
	 * @return An  object that contains host information for the address specified in .
	 */
	static function GetHostByName(hostName:String):cs.system.net.IPHostEntry;
	@:overload(function(address:cs.system.net.IPAddress):cs.system.net.IPHostEntry {})
	/**
	 * Resolves an IP address to an  instance.
	 * @param address An IP address.
	 * @return An  instance that contains address information about the host specified
	 * in .
	 */
	static function GetHostEntry(hostNameOrAddress:String):cs.system.net.IPHostEntry;
	@:overload(function(address:cs.system.net.IPAddress):cs.system.threading.tasks.Task_1<cs.system.net.IPHostEntry> {})
	/**
	 * Resolves an IP address to an  instance as an asynchronous operation.
	 * @param address An IP address.
	 * @return The task object representing the asynchronous operation. The  property
	 * on the task object returns an  instance that contains address information about
	 * the host specified in .
	 */
	static function GetHostEntryAsync(hostNameOrAddress:String):cs.system.threading.tasks.Task_1<cs.system.net.IPHostEntry>;
	/**
	 * Gets the host name of the local computer.
	 * @return A string that contains the DNS host name of the local computer.
	 */
	static function GetHostName():String;
	/**
	 * Resolves a DNS host name or IP address to an  instance.
	 * @param hostName A DNS-style host name or IP address.
	 * @return An  instance that contains address information about the host specified
	 * in .
	 */
	static function Resolve(hostName:String):cs.system.net.IPHostEntry;
}
