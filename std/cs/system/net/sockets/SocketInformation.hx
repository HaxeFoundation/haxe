package cs.system.net.sockets;

/** Encapsulates the information that is necessary to duplicate a . */
@:native("System.Net.Sockets.SocketInformation")
extern class SocketInformation extends cs.system.ValueType {
	/**
	 * Gets or sets the options for a .
	 * @return A  instance.
	 */
	var Options(default, default):cs.system.net.sockets.SocketInformationOptions;
	/**
	 * Gets or sets the protocol information for a .
	 * @return An array of type .
	 */
	var ProtocolInformation(default, default):cs.NativeArray<cs.UInt8>;
}
