package asys.net;

import asys.uv.UVError;
import haxe.NoData;
import haxe.signals.Signal;
import haxe.signals.ArraySignal;
import haxe.io.Bytes;
import asys.net.SocketOptions.UdpSocketOptions;


class UdpSocket {
	public static function create(type:IpFamily, ?options:UdpSocketOptions, ?listener:Listener<UdpMessage>):UdpSocket {
		var res = new UdpSocket(type);
		// TODO: use other options, register listener
		if (options == null)
			options = {};
		if (options.recvBufferSize != null)
			res.recvBufferSize = options.recvBufferSize;
		if (options.sendBufferSize != null)
			res.sendBufferSize = options.sendBufferSize;
		return res;
	}

	public final type:IpFamily;

	/**
		Remote address and port that `this` socket is connected to. See `connect`.
	**/
	public var remoteAddress(default, null):Null<SocketAddress>;

	private function get_localAddress():Null<SocketAddress> {
		return try native.getSockName() catch (e:Dynamic) null;
	}

	public var localAddress(get, never):Null<SocketAddress>;

	extern function get_recvBufferSize():Int;

	extern function set_recvBufferSize(size:Int):Int;

	public var recvBufferSize(get, set):Int;

	extern function get_sendBufferSize():Int;

	extern function set_sendBufferSize(size:Int):Int;

	public var sendBufferSize(get, set):Int;

	// final closeSignal:Signal<NoData>;
	// final connectSignal:Signal<NoData>;
	// final listeningSignal:Signal<NoData>;

	public var errorSignal(get,never):Signal<UVError>;
	final _errorSignal = new ArraySignal();
	inline function get_errorSignal():Signal<UVError>
		return _errorSignal;

	/**
		Emitted when a message is received by `this` socket. See `UdpMessage`.
	**/
	public var messageSignal(get,never):Signal<UdpMessage>;
	final _messageSignal = new ArraySignal();
	inline function get_messageSignal():Signal<UdpMessage>
		return _messageSignal;

	/**
		Joins the given multicast group.
	**/
	extern public function addMembership(multicastAddress:String, ?multicastInterface:String):Void;

	/**
		Leaves the given multicast group.
	**/
	extern public function dropMembership(multicastAddress:String, ?multicastInterface:String):Void;

	/**
		Binds `this` socket to a local address and port. Packets sent to the bound
		address will arrive via `messageSignal`. Outgoing packets will be sent from
		the given address and port. If any packet is sent without calling `bind`
		first, an address and port is chosen automatically by the system - it can
		be obtained with `localAddress`.
	**/
	extern public function bind(?address:Address, ?port:Int):Void;

	/**
		Closes `this` socket and all underlying resources.
	**/
	extern public function close(?cb:Callback<NoData>):Void;

	/**
		Connects `this` socket to a remote address and port. Any `send` calls after
		`connect` is called must not specify `address` nor `port`, they will
		automatically use the ones specified in the `connect` call.
	**/
	public function connect(?address:Address, port:Int):Void {
		if (remoteAddress != null)
			throw "already connected";
		if (address == null)
			address = AddressTools.localhost(type);
		remoteAddress = Network(address, port);
	}

	/**
		Clears any remote address and port previously set with `connect`.
	**/
	public function disconnect():Void {
		if (remoteAddress == null)
			throw "not connected";
		remoteAddress = null;
	}

	/**
		Sends a message.

		@param msg Buffer from which to read the message data.
		@param offset Position in `msg` at which to start reading.
		@param length Length of message in bytes.
		@param address Address to send the message to. Must be `null` if `this`
			socket is connected.
		@param port Port to send the message to. Must be `null` if `this` socket is
			connected.
	**/
	extern public function send(msg:Bytes, offset:Int, length:Int, ?address:Address, ?port:Int, ?cb:Callback<NoData>):Void;

	/**
		Sets broadcast on or off.
	**/
	extern public function setBroadcast(flag:Bool):Void;

	/**
		Sets the multicast interface on which to send and receive data.
	**/
	extern public function setMulticastInterface(multicastInterface:String):Void;

	/**
		Set IP multicast loopback on or off. Makes multicast packets loop back to
		local sockets.
	**/
	extern public function setMulticastLoopback(flag:Bool):Void;

	/**
		Sets the multicast TTL (time-to-live).
	**/
	extern public function setMulticastTTL(ttl:Int):Void;

	/**
		Sets the TTL (time-to-live) for outgoing packets.

		@param ttl Number of hops.
	**/
	extern public function setTTL(ttl:Int):Void;

	extern public function ref():Void;

	extern public function unref():Void;

	function new(type) {
		this.type = type;
	}
}

/**
	A packet received emitted by `messageSignal` of a `UdpSocket`.
**/
typedef UdpMessage = {
	/**
		Message data.
	**/
	var data:Bytes;
	/**
		Remote IPv4 or IPv6 address from which the message originates.
	**/
	var remoteAddress:Address;
	/**
		Remote port from which the message originates.
	**/
	var remotePort:Int;
};
