package asys.net;

import haxe.Error;
import haxe.NoData;
import haxe.async.*;
import haxe.io.*;
import haxe.io.Readable.ReadResult;
import asys.io.*;
import asys.net.SocketOptions.SocketConnectTcpOptions;
import asys.net.SocketOptions.SocketConnectIpcOptions;

private typedef NativeStream =
	#if doc_gen
	Void;
	#elseif eval
	eval.uv.Stream;
	#elseif hl
	hl.uv.Stream;
	#elseif neko
	neko.uv.Stream;
	#else
	#error "socket not supported on this platform"
	#end

private typedef NativeSocket =
	#if doc_gen
	Void;
	#elseif eval
	eval.uv.Socket;
	#elseif hl
	hl.uv.Socket;
	#elseif neko
	neko.uv.Socket;
	#else
	#error "socket not supported on this platform"
	#end

private typedef NativePipe =
	#if doc_gen
	Void;
	#elseif eval
	eval.uv.Pipe;
	#elseif hl
	hl.uv.Pipe;
	#elseif neko
	neko.uv.Pipe;
	#else
	#error "socket not supported on this platform"
	#end

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
	public final closeSignal:Signal<NoData> = new ArraySignal();

	public final connectSignal:Signal<NoData> = new ArraySignal();

	// endSignal

	/**
		(TCP only.) Emitted after the IP address of the hostname given in
		`connectTcp` is resolved, but before the socket connects.
	**/
	public final lookupSignal:Signal<Address> = new ArraySignal();

	/**
		Emitted when a timeout occurs. See `setTimeout`.
	**/
	public final timeoutSignal:Signal<NoData> = new ArraySignal();

	private function get_localAddress():Null<SocketAddress> {
		if (nativeSocket != null)
			return nativeSocket.getSockName();
		if (nativePipe != null)
			return nativePipe.getSockName();
		return null;
	}

	/**
		The address of the local side of the socket connection, or `null` if not
		connected.
	**/
	public var localAddress(get, never):Null<SocketAddress>;

	private function get_remoteAddress():Null<SocketAddress> {
		if (nativeSocket != null)
			return nativeSocket.getPeerName();
		if (nativePipe != null)
			return nativePipe.getPeerName();
		return null;
	}

	/**
		The address of the remote side of the socket connection, or `null` if not
		connected.
	**/
	public var remoteAddress(get, never):Null<SocketAddress>;

	private function get_handlesPending():Int {
		if (nativePipe == null)
			throw "not connected via IPC";
		return nativePipe.pendingCount();
	}

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
	public function connectTcp(options:SocketConnectTcpOptions, ?cb:Callback<NoData>):Void {
		if (connectStarted || connected)
			throw "already connected";

		if (options.host != null && options.address != null)
			throw "cannot specify both host and address";

		connectStarted = true;
		nativeSocket = new NativeSocket();
		native = nativeSocket.asStream();

		// take a copy since we reuse the object asynchronously
		var options = {
			port: options.port,
			host: options.host,
			address: options.address,
			localAddress: options.localAddress,
			localPort: options.localPort,
			family: options.family
		};

		function connect(address:Address):Void {
			connectDefer = null;
			// TODO: bindTcp for localAddress and localPort, if specified
			try {
				nativeSocket.connectTcp(address, options.port, (err, nd) -> {
					timeoutReset();
					if (err == null)
						connected = true;
					if (cb != null)
						cb(err, nd);
					if (err == null)
						connectSignal.emit(new NoData());
				});
			} catch (err:haxe.Error) {
				if (cb != null)
					cb(err, new NoData());
			}
		}

		if (options.address != null) {
			connectDefer = Defer.nextTick(() -> connect(options.address));
			return;
		}
		if (options.host == null)
			options.host = "localhost";
		Dns.lookup(options.host, {family: options.family}, (err, entries) -> {
			timeoutReset();
			if (err != null)
				return errorSignal.emit(err);
			if (entries.length == 0)
				throw "!";
			lookupSignal.emit(entries[0]);
			connect(entries[0]);
		});
	}

	/**
		Connect `this` socket to an IPC pipe.

		@param options.path Pipe path.
	**/
	public function connectIpc(options:SocketConnectIpcOptions, ?cb:Callback<NoData>):Void {
		if (connectStarted || connected)
			throw "already connected";

		connectStarted = true;
		nativePipe = new NativePipe(false);
		native = nativePipe.asStream();

		try {
			nativePipe.connectIpc(options.path, (err, nd) -> {
				timeoutReset();
				if (err == null)
					connected = true;
				if (cb != null)
					cb(err, nd);
				if (err == null)
					connectSignal.emit(new NoData());
			});
		} catch (err:haxe.Error) {
			if (cb != null)
				cb(err, new NoData());
		}
	}

	/**
		Connect `this` socket to a file descriptor. Used internally to establish
		IPC channels between Haxe processes.

		@param ipc Whether IPC features (sending sockets) should be enabled.
	**/
	public function connectFd(ipc:Bool, fd:Int):Void {
		if (connectStarted || connected)
			throw "already connected";

		connectStarted = true;
		nativePipe = new NativePipe(ipc);
		nativePipe.open(fd);
		connected = true;
		native = nativePipe.asStream();

		// TODO: signal consistency with other connect methods
	}

	/**
		Closes `this` socket and all underlying resources.
	**/
	public function destroy(?cb:Callback<NoData>):Void {
		if (readStarted)
			native.stopRead();
		native.close((err, nd) -> {
			if (err != null)
				errorSignal.emit(err);
			if (cb != null)
				cb(err, nd);
			closeSignal.emit(new NoData());
		});
	}

	/**
		(TCP only.) Enable or disable TCP keep-alive.

		@param initialDelay Initial delay in seconds. Ignored if `enable` is
			`false`.
	**/
	public function setKeepAlive(?enable:Bool = false, ?initialDelay:Int = 0):Void {
		if (nativeSocket == null)
			throw "not connected via TCP";
		nativeSocket.setKeepAlive(enable, initialDelay);
	}

	/**
		(TCP only.) Enable or disable TCP no-delay. Enabling no-delay disables
		Nagle's algorithm.
	**/
	public function setNoDelay(?noDelay:Bool = true):Void {
		if (nativeSocket == null)
			throw "not connected via TCP";
		nativeSocket.setNoDelay(noDelay);
	}

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
	public function writeHandle(data:Bytes, handle:Socket):Void {
		if (nativePipe == null)
			throw "not connected via IPC";
		nativePipe.writeHandle(data, handle.native, writeDone);
	}

	/**
		(IPC only.) Receive a socket or pipe. Should only be called when
		`handlesPending` is greater than zero.
	**/
	public function readHandle():Socket {
		if (nativePipe == null)
			throw "not connected via IPC";
		var ret = new Socket();
		switch (nativePipe.acceptPending()) {
			case Socket(nativeSocket):
				ret.nativeSocket = nativeSocket;
				ret.native = nativeSocket.asStream();
			case Pipe(nativePipe):
				ret.nativePipe = nativePipe;
				ret.native = nativePipe.asStream();
		}
		ret.connected = true;
		return ret;
	}

	public function ref():Void {
		if (native == null)
			throw "not connected";
		native.ref();
	}

	public function unref():Void {
		if (native == null)
			throw "not connected";
		native.unref();
	}

	var connectDefer:haxe.Timer;
	var native:NativeStream;
	var nativeSocket:NativeSocket;
	var nativePipe:NativePipe;
	var internalReadCalled = false;
	var readStarted = false;
	var connectStarted = false;
	var serverSpawn:Bool = false;
	var timeoutTime:Int = 0;
	var timeoutTimer:haxe.Timer;

	function new() {
		super();
	}

	function initPipe(ipc:Bool):Void {
		nativePipe = new NativePipe(ipc);
		native = nativePipe.asStream();
		connected = true;
	}

	override function internalRead(remaining):ReadResult {
		if (internalReadCalled)
			return None;
		internalReadCalled = true;

		function start():Void {
			readStarted = true;
			native.startRead((err, chunk) -> {
				timeoutReset();
				if (err != null) {
					switch (err.type) {
						case UVError(EOF):
							asyncRead([], true);
						case _:
							errorSignal.emit(err);
					}
				} else {
					asyncRead([chunk], false);
				}
			});
		}

		if (connected)
			start();
		else
			connectSignal.once(start);

		return None;
	}

	// TODO: keep track of pending writes for finish event emission
	// in `internalWrite` and `writeHandle`
	function writeDone(err:Error, nd:NoData):Void {
		timeoutReset();
		if (err != null)
			errorSignal.emit(err);
		// TODO: destroy stream and socket
	}

	override function internalWrite():Void {
		while (inputBuffer.length > 0) {
			native.write(pop(), writeDone);
		}
	}

	function timeoutTrigger():Void {
		timeoutTimer = null;
		timeoutSignal.emit(new NoData());
	}

	function timeoutReset():Void {
		if (timeoutTimer != null)
			timeoutTimer.stop();
		timeoutTimer = null;
		if (timeoutTime != 0) {
			timeoutTimer = haxe.Timer.delay(timeoutTrigger, timeoutTime);
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
