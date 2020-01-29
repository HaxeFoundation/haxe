package asyncio.net;

import asyncio.net.SocketOption.SocketOptionKind;
import haxe.io.Bytes;
import haxe.NoData;
import haxe.errors.NotImplemented;
import haxe.Callback;

class UdpSocket {
	/**
		Indicates if the socket is currently bound to a specific remote address.
	**/
	public var bound(get,never):Bool;
	function get_bound():Bool throw new NotImplemented();

	/**
		Local address of this socket.
	**/
	public var localAddress(get,never):{host:String, port:Int};
	function get_localAddress():{host:String, port:Int} throw new NotImplemented();

	/**
		Remote address of this socket if it is bound.
	**/
	public var remoteAddress(get,never):Null<{host:String, port:Int}>;
	function get_remoteAddress():Null<{host:String, port:Int}> throw new NotImplemented();

	/**
		Open a UDP socket.
	**/
	static public function open(?address:{host:String, port:Int}, ?options:Array<SocketOption>, callback:Callback<Null<UdpSocket>>) {
		callback.fail(new NotImplemented());
	}

	/**
		Bind the socket to the `host` and `port`.
		The callback is supplied with the new write function, which allows to send
		data without the need to specify remote address on each call.
	**/
	public function bind(host:String, port:Int, callback:Callback<Null< (buffer:Bytes, offset:Int, length:Int)->Void >>) {
		callback.fail(new NotImplemented());
	}

	/**
		Unbind previously bound socket.
	**/
	public function unbind(callback:Callback<NoData>) {
		callback.fail(new NotImplemented());
	}

	/**
		Send up to `length` bytes from `buffer` (starting from buffer `offset`) to
		the remote `host`.
		The `callback` is supplied with the amount of bytes sent.
	**/
	public function write(buffer:Bytes, offset:Int, length:Int, host:String, port:Int, callback:Callback<Int>) {
		callback.fail(new NotImplemented());
	}

	/**
		Read up to `length` bytes and write them into `buffer` starting from `offset`
		position in `buffer`.
		The `callback` is supplied with the amount of bytes read and the peer address.

		If `recycle` is `true` then the structure passed to `callback` will be reused
		instead of allocating a new one on the next read call with recycling enabled.
	**/
	public function read(buffer:Bytes, offset:Int, length:Int, recycle:Bool = false, callback:Callback<Null<{bytesReceived:Int, remoteHost:Ip, remotePort:Int}>>) {
		callback.fail(new NotImplemented());
	}

	/**
		Get the value of a specified socket option.
	**/
	public function getOption<T>(option:SocketOptionKind<T>, callback:Callback<Null<T>>) {
		callback.fail(new NotImplemented());
	}

	/**
		Set socket option.
	**/
	public function setOption(option:SocketOption, callback:Callback<NoData>) {
		callback.fail(new NotImplemented());
	}

	/**
		Close the socket.
	**/
	public function close(callback:Callback<NoData>) {
		callback.fail(new NotImplemented());
	}
}