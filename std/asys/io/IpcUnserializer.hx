package asys.io;

import haxe.NoData;
import haxe.async.*;
import haxe.io.*;
import asys.net.Socket;

/**
	Class used internally to receive messages and handles over an IPC channel.
	See `CurrentProcess.initIpc` for initialising IPC for a process.
**/
class IpcUnserializer {
	static var activeUnserializer:IpcUnserializer = null;

	public final messageSignal:Signal<IpcMessage> = new ArraySignal();
	public final errorSignal:Signal<Dynamic> = new ArraySignal();

	final pipe:Socket;
	// var chunkSockets:Array<Socket> = [];
	var chunkLenbuf:String = "";
	var chunkBuf:StringBuf;
	var chunkSize:Null<Int> = 0;
	var chunkSocketCount:Int = 0;

	function new(pipe:Socket) {
		this.pipe = pipe;
		pipe.dataSignal.on(handleData);
	}

	function handleData(data:Bytes):Void {
		if (data.length == 0)
			return;
		try {
			var data = data.toString();
			while (data != null) {
				if (chunkSize == 0) {
					chunkLenbuf += data;
					var colonPos = chunkLenbuf.indexOf(":");
					if (colonPos != -1) {
						chunkSocketCount = 0;
						while (chunkLenbuf.charAt(chunkSocketCount) == "s")
							chunkSocketCount++;
						chunkSize = Std.parseInt(chunkLenbuf.substr(chunkSocketCount, colonPos));
						if (chunkSize == null || chunkSize <= 0) {
							chunkSize = 0;
							throw "invalid chunk size received";
						}
						chunkBuf = new StringBuf();
						chunkBuf.add(chunkLenbuf.substr(colonPos + 1));
						chunkLenbuf = "";
						// chunkSockets.resize(0);
					}
				} else {
					chunkBuf.add(data);
				}
				data = null;
				if (chunkSize != 0) {
					if (chunkBuf.length >= chunkSize) {
						var serial = chunkBuf.toString();
						if (serial.length > chunkSize) {
							data = serial.substr(chunkSize);
							serial = serial.substr(0, chunkSize);
						}
						chunkBuf = null;
						var chunkSockets = [];
						if (chunkSocketCount > pipe.handlesPending)
							throw "not enough handles received";
						for (i in 0...chunkSocketCount)
							chunkSockets.push(pipe.readHandle());
						activeUnserializer = this;
						var message = haxe.Unserializer.run(serial);
						messageSignal.emit({message: message, sockets: chunkSockets});
						chunkSize = 0;
						chunkSocketCount = 0;
						// chunkSockets.resize(0);
						activeUnserializer = null;
					}
				}
			}
		} catch (e:Dynamic) {
			errorSignal.emit(e);
		}
	}
}
