
package cs.system.net.sockets;

import cs.system.io.Stream;

@:native("System.Net.Sockets.NetworkStream") 
extern class NetworkStream extends Stream{
	public function new( socket : Socket );
	
	/**
	 * This method reads data into the buffer parameter and returns the number of bytes successfully read. 
	 * If no data is available for reading, the Read method returns 0.
	 * The Read operation reads as much data as is available, up to the number of bytes specified by the size parameter.
	 */
	public override function Read( buffer : haxe.io.BytesData, offset : Int, size : Int ) : Int;
	
	/**
	 * The Write method starts at the specified offset and sends size bytes from the contents of buffer to the network. 
	 * The Write method blocks until the requested number of bytes is sent or a SocketException is thrown.
	 */
	public override function Write( buffer : haxe.io.BytesData, offset : Int, size : Int ) : Void;
}
