package asys.io;

import haxe.Error;
import haxe.NoData;
import haxe.async.*;
import haxe.io.*;
import asys.net.Socket;

/**
	Class used internally to send messages and handles over an IPC channel. See
	`Process.spawn` for creating an IPC channel and `Process.send` for sending
	messages over the channel.
**/
class IpcSerializer {
	static var activeSerializer:IpcSerializer = null;
	static var dummyBuffer = Bytes.ofString("s");

	final pipe:Socket;
	// final chunkSockets:Array<Socket> = [];

	function new(pipe:Socket) {
		this.pipe = pipe;
	}

	/**
		Sends `data` over the pipe. `data` will be serialized with a call to
		`haxe.Serializer.run`. Objects of type `Socket` can be sent along with the
		data if `handles` is provided.
	**/
	public function write(message:IpcMessage):Void {
		activeSerializer = this;
		if (message.sockets != null)
			for (socket in message.sockets) {
				if (!socket.connected)
					throw "cannot send unconnected socket over IPC";
				pipe.writeHandle(dummyBuffer, socket);
			}
		var serial = haxe.Serializer.run(message.message);
		pipe.write(Bytes.ofString('${serial.length}:$serial'));
		// chunkSockets.resize(0);
		activeSerializer = null;
	}

	/**
		// TODO: see `Socket.hxUnserialize` comment
		Sends `data` over the pipe. `data` will be serialized with a call to
		`haxe.Serializer.run`. However, objects of type `asys.async.net.Socket`
		will also be correctly serialized and can be received by the other end.
	**/
}
