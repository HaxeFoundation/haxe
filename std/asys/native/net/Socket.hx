package asys.native.net;

import asys.native.net.SocketOptions.SocketOptionKind;
import haxe.NoData;
import haxe.io.Bytes;
import haxe.Callback;
import haxe.exceptions.NotImplementedException;

class Socket implements IDuplex {
	/**
		Local address of this socket.
	**/
	public var localAddress(get,never):SocketAddress;
	function get_localAddress():SocketAddress throw new NotImplementedException();

	/**
		Remote address of this socket if it is bound.
	**/
	public var remoteAddress(get,never):Null<SocketAddress>;
	function get_remoteAddress():Null<SocketAddress> throw new NotImplementedException();

	/**
		Establish a connection to `address`.
	**/
	static public function connect(address:SocketAddress, ?options:SocketOptions, callback:Callback<Null<Socket>>) {
		callback.fail(new NotImplementedException());
	}

	/**
		Read up to `length` bytes and write them into `buffer` starting from `offset`
		position in `buffer`, then invoke `callback` with the amount of bytes read.
	**/
	public function read(buffer:Bytes, offset:Int, length:Int, callback:Callback<Int>) {
		callback.fail(new NotImplementedException());
	}

	/**
		Write up to `length` bytes from `buffer` (starting from buffer `offset`),
		then invoke `callback` with the amount of bytes written.
	**/
	public function write(buffer:Bytes, offset:Int, length:Int, callback:Callback<Int>) {
		callback.fail(new NotImplementedException());
	}

	/**
		Force all buffered data to be committed.
	**/
	public function flush(callback:Callback<NoData>):Void {
		callback.fail(new NotImplementedException());
	}

	/**
		Get the value of a specified socket option.
	**/
	public function getOption<T>(option:SocketOptionKind<T>, callback:Callback<Null<T>>) {
		callback.fail(new NotImplementedException());
	}

	/**
		Set socket option.
	**/
	public function setOption<T>(option:SocketOptionKind<T>, value:T, callback:Callback<NoData>) {
		callback.fail(new NotImplementedException());
	}

	/**
		Close the connection.
	**/
	public function close(callback:Callback<NoData>) {
		callback.fail(new NotImplementedException());
	}
}