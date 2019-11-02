package asys;

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
	public static final messageSignal:Signal<IpcMessage> = new ArraySignal();

	static var ipc:Socket;
	static var ipcOut:IpcSerializer;
	static var ipcIn:IpcUnserializer;

	/**
		Environment variables, as available to the process when it was created.
		This map can be modified, but the changes will only be visible within the
		current process. Use `asys.System.setEnv` to make modifications which will
		be available in other processes in the same shell.
	**/
	public static var environment(default, null):Map<String, String>;

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
		ipc.connectFd(fd, true);
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
