package asys;

import haxe.signals.Signal;
import haxe.signals.ArraySignal;
import haxe.async.*;
import asys.net.Socket;
import asys.io.*;

/**
	Methods to control the current process and IPC interaction with the parent
	process.
**/
class CurrentProcess {
	/**
		Emitted when a message is received over IPC. `initIpc` must be called first
		to initialise the IPC channel.
	**/
	public static var messageSignal(get,never):Signal<IpcMessage>;
	static final _messageSignal = new ArraySignal<IpcMessage>();
	static inline function get_messageSignal():Signal<IpcMessage>
		return _messageSignal;


	static var ipc:Socket;
	static var ipcOut:IpcSerializer;
	static var ipcIn:IpcUnserializer;

	/**
		Initialise the IPC channel on the given file descriptor `fd`. This should
		only be used when the current process was spawned with `Process.spawn` from
		another Haxe process. `fd` should correspond to the index of the `Ipc`
		entry in `options.stdio`.
	**/
	public static function initIpc(fd:Int):Void {
		if (ipc != null)
			throw "IPC already initialised";
		ipc = Socket.create();
		ipcOut = @:privateAccess new IpcSerializer(ipc);
		ipcIn = @:privateAccess new IpcUnserializer(ipc);
		ipc.connectFd(true, fd);
		ipc.errorSignal.on(err -> trace("IPC error", err));
		ipcIn.messageSignal.on(message -> messageSignal.emit(message));
	}

	/**
		Sends a message over IPC. `initIpc` must be called first to initialise the
		IPC channel.
	**/
	public static function send(message:IpcMessage):Void {
		if (ipc == null)
			throw "IPC not connected";
		ipcOut.write(message);
	}

	extern public static function initUv():Void;

	extern public static function runUv(?mode:asys.uv.UVRunMode = RunDefault):Bool;

	extern public static function stopUv():Void;
}
