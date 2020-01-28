package asyncio.net;

import asyncio.net.SocketOption.SocketOptionKind;
import asyncio.net.Server.ServerAddress;
import haxe.NoData;
import haxe.io.Bytes;
import haxe.Callback;
import haxe.errors.NotImplemented;

class Socket implements IDuplex {
	/**
		Establish a connection to `address`.
	**/
	static public function connect(address:ServerAddress, ?options:Array<SocketOption>, callback:Callback<Null<Socket>>) {
		callback.fail(new NotImplemented());
	}

	/**
		Read up to `length` bytes and write them into `buffer` starting from `offset`
		position in `buffer`, then invoke `callback` with the amount of bytes read.
	**/
	public function read(buffer:Bytes, offset:Int, length:Int, callback:Callback<Int>) {
		callback.fail(new NotImplemented());
	}

	/**
		Write up to `length` bytes from `buffer` (starting from buffer `offset`),
		then invoke `callback` with the amount of bytes written.
	**/
	public function write(buffer:Bytes, offset:Int, length:Int, callback:Callback<Int>) {
		callback.fail(new NotImplemented());
	}

	/**
		Get the value of a specified socket option.
	**/
	public function getOption<T>(option:SocketOptionKind<T>, callback:Callback<Null<T>>) {
		callback.fail(new NotImplemented());
	}

	/**
		Close the connection.
	**/
	public function close(callback:Callback<NoData>) {
		callback.fail(new NotImplemented());
	}
}