package asys.net;

import haxe.Error;
import haxe.NoData;
import haxe.async.*;
import haxe.io.Bytes;
import asys.net.SocketOptions.UdpSocketOptions;

private typedef Native =
	#if doc_gen
	Void;
	#elseif eval
	eval.uv.UdpSocket;
	#elseif hl
	hl.uv.UdpSocket;
	#elseif neko
	neko.uv.UdpSocket;
	#else
	#error "UDP socket not supported on this platform"
	#end

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

	private function get_recvBufferSize():Int {
		return native.getRecvBufferSize();
	}

	private function set_recvBufferSize(size:Int):Int {
		return native.setRecvBufferSize(size);
	}

	public var recvBufferSize(get, set):Int;

	private function get_sendBufferSize():Int {
		return native.getSendBufferSize();
	}

	private function set_sendBufferSize(size:Int):Int {
		return native.setSendBufferSize(size);
	}

	public var sendBufferSize(get, set):Int;

	// final closeSignal:Signal<NoData>;
	// final connectSignal:Signal<NoData>;
	// final listeningSignal:Signal<NoData>;

	public final errorSignal:Signal<Error> = new ArraySignal();

	/**
		Emitted when a message is received by `this` socket. See `UdpMessage`.
	**/
	public final messageSignal:Signal<UdpMessage> = new ArraySignal();

	/**
		Joins the given multicast group.
	**/
	public function addMembership(multicastAddress:String, ?multicastInterface:String):Void {
		if (multicastInterface == null)
			multicastInterface = "";
		native.addMembership(multicastAddress, multicastInterface);
	}

	/**
		Leaves the given multicast group.
	**/
	public function dropMembership(multicastAddress:String, ?multicastInterface:String):Void {
		if (multicastInterface == null)
			multicastInterface = "";
		native.dropMembership(multicastAddress, multicastInterface);
	}

	/**
		Binds `this` socket to a local address and port. Packets sent to the bound
		address will arrive via `messageSignal`. Outgoing packets will be sent from
		the given address and port. If any packet is sent without calling `bind`
		first, an address and port is chosen automatically by the system - it can
		be obtained with `localAddress`.
	**/
	public function bind(?address:Address, ?port:Int):Void {
		if (address == null)
			address = AddressTools.all(type);
		if (port == null)
			port = 0;
		native.bindTcp(address, port, false);
		native.startRead((err, msg) -> {
			if (err != null)
				return errorSignal.emit(err);
			messageSignal.emit(msg);
		});
	}

	/**
		Closes `this` socket and all underlying resources.
	**/
	public function close(?cb:Callback<NoData>):Void {
		native.stopRead();
		native.close(Callback.nonNull(cb));
	}

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
	public function send(msg:Bytes, offset:Int, length:Int, ?address:Address, ?port:Int, ?cb:Callback<NoData>):Void {
		if (address == null && port == null) {
			if (remoteAddress == null)
				throw "not connected";
		} else if (address != null && port != null) {
			if (remoteAddress != null)
				throw "already connected";
		} else
			throw "invalid arguments";
		if (address == null) {
			switch (remoteAddress) {
				case Network(a, p):
					address = a;
					port = p;
				case _:
					throw "!";
			}
		}
		native.send(msg, offset, length, address, port, cb);
	}

	/**
		Sets broadcast on or off.
	**/
	public function setBroadcast(flag:Bool):Void {
		native.setBroadcast(flag);
	}

	/**
		Sets the multicast interface on which to send and receive data.
	**/
	public function setMulticastInterface(multicastInterface:String):Void {
		native.setMulticastInterface(multicastInterface);
	}

	/**
		Set IP multicast loopback on or off. Makes multicast packets loop back to
		local sockets.
	**/
	public function setMulticastLoopback(flag:Bool):Void {
		native.setMulticastLoopback(flag);
	}

	/**
		Sets the multicast TTL (time-to-live).
	**/
	public function setMulticastTTL(ttl:Int):Void {
		native.setMulticastTTL(ttl);
	}

	/**
		Sets the TTL (time-to-live) for outgoing packets.

		@param ttl Number of hops.
	**/
	public function setTTL(ttl:Int):Void {
		native.setTTL(ttl);
	}

	public function ref():Void {
		native.asStream().ref();
	}

	public function unref():Void {
		native.asStream().unref();
	}

	var native:Native;

	function new(type) {
		native = new Native();
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
