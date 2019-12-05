package asys.net;

import asys.uv.UVError;
import haxe.NoData;
import haxe.signals.Signal;
import haxe.signals.ArraySignal;
import haxe.io.*;
import haxe.io.Readable.ReadResult;
import asys.io.*;
import asys.net.SocketOptions.SocketConnectTcpOptions;
import asys.net.SocketOptions.SocketConnectIpcOptions;

/**
	Socket object, used for clients and servers for TCP communications and IPC
	(inter-process communications) over Windows named pipes and Unix local domain
	sockets.

	An IPC pipe is a communication channel between two processes. It may be
	uni-directional or bi-directional, depending on how it is created. Pipes can
	be automatically created for spawned subprocesses with `Process.spawn`.
**/
class Socket extends Duplex {
	/**
		Creates an unconnected socket or pipe instance.

		@param options.allowHalfOpen
		@param options.readable Whether the socket should be readable to the
			current process.
		@param options.writable Whether the socket should be writable to the
			current process.
	**/
	public static function create(?options:SocketOptions):Socket {
		// TODO: use options
		return new Socket();
	}

	/**
		Emitted when the socket connects to a remote endpoint.
	**/
	public var closeSignal(get,never):Signal<NoData>;
	final _closeSignal = new ArraySignal();
	inline function get_closeSignal():Signal<NoData>
		return _closeSignal;

	public var connectSignal(get,never):Signal<NoData>;
	final _connectSignal = new ArraySignal();
	inline function get_connectSignal():Signal<NoData>
		return _connectSignal;

	// endSignal

	/**
		(TCP only.) Emitted after the IP address of the hostname given in
		`connectTcp` is resolved, but before the socket connects.
	**/
	public var lookupSignal(get,never):Signal<Address>;
	final _lookupSignal = new ArraySignal();
	inline function get_lookupSignal():Signal<Address>
		return _lookupSignal;

	/**
		Emitted when a timeout occurs. See `setTimeout`.
	**/
	public var timeoutSignal(get,never):Signal<NoData>;
	final _timeoutSignal = new ArraySignal();
	inline function get_timeoutSignal():Signal<NoData>
		return _timeoutSignal;

	extern private function get_localAddress():Null<SocketAddress>;

	/**
		The address of the local side of the socket connection, or `null` if not
		connected.
	**/
	public var localAddress(get, never):Null<SocketAddress>;

	extern private function get_remoteAddress():Null<SocketAddress>;

	/**
		The address of the remote side of the socket connection, or `null` if not
		connected.
	**/
	public var remoteAddress(get, never):Null<SocketAddress>;

	extern private function get_handlesPending():Int;

	/**
		(IPC only.) Number of pending sockets or pipes. Accessible using
		`readHandle`.
	**/
	public var handlesPending(get, never):Int;

	/**
		`true` when `this` socket is connected to a remote host or an IPC pipe.
	**/
	public var connected(default, null):Bool = false;

	/**
		Connect `this` socket via TCP to the given remote.

		If neither `options.host` nor `options.address` is specified, the host
		`localhost` is resolved via DNS and used as the address. At least one of
		`options.host` or `options.address` must be `null`.

		`options.localAddress` and `options.localPort` can be used to specify what
		address and port to use on the local machine for the outgoing connection.
		If `null` or not specified, an address and/or a port will be chosen
		automatically by the system when connecting. The local address and port can
		be obtained using the `localAddress`.

		@param options.port Remote port to connect to.
		@param options.host Hostname to connect to, will be resolved using
			`Dns.resolve` to an address. `lookupSignal` will be emitted with the
			resolved address before the connection is attempted.
		@param options.address IPv4 or IPv6 address to connect to.
		@param options.localAddress Local IPv4 or IPv6 address to connect from.
		@param options.localPort Local port to connect from.
		@param options.family Limit DNS lookup to the given family.
	**/
	extern public function connectTcp(options:SocketConnectTcpOptions, ?cb:Callback<NoData>):Void;

	/**
		Connect `this` socket to an IPC pipe.

		@param options.path Pipe path.
	**/
	extern public function connectIpc(options:SocketConnectIpcOptions, ?cb:Callback<NoData>):Void;

	/**
		Connect `this` socket to a file descriptor. Used internally to establish
		IPC channels between Haxe processes.

		@param ipc Whether IPC features (sending sockets) should be enabled.
	**/
	extern public function connectFd(ipc:Bool, fd:Int):Void;

	/**
		Closes `this` socket and all underlying resources.
	**/
	extern public function destroy(?cb:Callback<NoData>):Void;

	/**
		(TCP only.) Enable or disable TCP keep-alive.

		@param initialDelay Initial delay in seconds. Ignored if `enable` is
			`false`.
	**/
	extern public function setKeepAlive(?enable:Bool = false, ?initialDelay:Int = 0):Void;

	/**
		(TCP only.) Enable or disable TCP no-delay. Enabling no-delay disables
		Nagle's algorithm.
	**/
	extern public function setNoDelay(?noDelay:Bool = true):Void;

	/**
		Set a timeout for socket oprations. Any time activity is detected on the
		socket (see below), the timer is reset to `timeout`. When the timer runs
		out, `timeoutSignal` is emitted. Note that a timeout will not automatically
		do anything to the socket - it is up to the `timeoutSignal` handler to
		perform an action, e.g. ping the remote host or close the socket.

		Socket activity which resets the timer:

		- A chunk of data is received.
		- An error occurs during reading.
		- A chunk of data is written to the socket.
		- Connection is established.
		- (TCP only.) DNS lookup is finished (successfully or not).

		@param timeout Timeout in seconds, or `0` to disable.
	**/
	public function setTimeout(timeout:Int, ?listener:Listener<NoData>):Void {
		timeoutTime = timeout;
		timeoutReset();
		if (listener != null)
			timeoutSignal.once(listener);
	}

	/**
		(IPC only.) Send a socket or pipe in along with the given `data`. The
		socket must be connected.
	**/
	extern public function writeHandle(data:Bytes, handle:Socket):Void;

	/**
		(IPC only.) Receive a socket or pipe. Should only be called when
		`handlesPending` is greater than zero.
	**/
	extern public function readHandle():Socket;

	extern public function ref():Void;

	extern public function unref():Void;

	var connectDefer:asys.Timer;
	var internalReadCalled = false;
	var readStarted = false;
	var connectStarted = false;
	var serverSpawn:Bool = false;
	var timeoutTime:Int = 0;
	var timeoutTimer:asys.Timer;

	function new() {
		super();
	}

	extern function initPipe(ipc:Bool):Void;

	// TODO: keep track of pending writes for finish event emission
	// in `internalWrite` and `writeHandle`
	function writeDone(err:UVError, nd:NoData):Void {
		timeoutReset();
		if (err != null)
			_errorSignal.emit(err);
		// TODO: destroy stream and socket
	}

	function timeoutTrigger():Void {
		timeoutTimer = null;
		_timeoutSignal.emit(new NoData());
	}

	function timeoutReset():Void {
		if (timeoutTimer != null)
			timeoutTimer.stop();
		timeoutTimer = null;
		if (timeoutTime != 0) {
			timeoutTimer = asys.Timer.delay(timeoutTrigger, timeoutTime);
			timeoutTimer.unref();
		}
	}

	/*
	// TODO: #8263 (static hxUnserialize)
	// Automatic un/serialisation will not work here since hxUnserialize needs to
	// call super, otherwise the socket is unusable; for now sockets are
	// delivered separately in IPC.

	@:access(asys.io.IpcSerializer)
	private function hxSerialize(_):Void {
		if (IpcSerializer.activeSerializer == null)
			throw "cannot serialize socket";
		IpcSerializer.activeSerializer.chunkSockets.push(this);
	}

	@:access(asys.io.IpcUnserializer)
	private function hxUnserialize(_):Void {
		if (IpcUnserializer.activeUnserializer == null)
			throw "cannot unserialize socket";
		trace(dataSignal, input);
		var source:Socket = IpcUnserializer.activeUnserializer.chunkSockets.shift();
		this.native = source.native;
		this.nativePipe = source.nativePipe;
		this.nativeSocket = source.nativeSocket;
		this.connected = true;
		trace("successfully unserialized", this.nativeSocket);
	}
	*/
}
