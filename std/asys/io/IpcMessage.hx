package asys.io;

import asys.net.Socket;

/**
	A message sent over an IPC channel. Sent with `Process.send` to a sub-process
	or with `CurrentProcess.send` to the parent process. Received with
	`Process.messageSignal` from a sub-process, or `CurrentProcess.messageSignal`
	from the parent process.
**/
typedef IpcMessage = {
	/**
		The actual message. May be any data that is serializable with
		`haxe.Serializer`.
	**/
	var message:Any;
	/**
		Sockets and pipes associated with the message. Must be connected.
	**/
	var ?sockets:Array<Socket>;
};
