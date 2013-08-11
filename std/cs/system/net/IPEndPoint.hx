
package cs.system.net;

@:native("System.Net.IPEndPoint") 
extern class IPEndPoint extends EndPoint {
	
	/**
	 * Initializes a new instance of the IPEndPoint class with the specified address and port number.
	 */
	public function new( address : IPAddress, port : Int);
	
	public var Address (default, default) : IPAddress;
	public var Port (default, default) : Int;
}