package eval.luv;

@:coreType abstract SocketHandle {}

/**
	Handles.

	@see https://aantron.github.io/luv/luv/Luv/Handle
**/
@:coreType abstract Handle {
	/**
		Closes the given handle.
	**/
	extern static public function close(handle:Handle, callback:()->Void):Void;

	/**
		Returns `true` if the handle is active, `false` otherwise.
	**/
	static public function isActive(handle:Handle):Bool;

	/**
		Returns `true` if the handle is closing or closed, `false` otherwise.

		Note: This function should only be used between the initialization of
		the handle and the arrival of the close callback.
	**/
	static public function isClosing(handle:Handle):Bool;

	/**
		Reference the given handle.

		@see https://aantron.github.io/luv/luv/Luv/Handle/#val-ref
	**/
	static public function ref(handle:Handle):Void;

	/**
		Un-reference the given handle.

		@see https://aantron.github.io/luv/luv/Luv/Handle/#val-unref
	**/
	static public function unref(handle:Handle):Void;

	/**
		Returns `true` if the handle referenced, `false` otherwise.

		@see https://aantron.github.io/luv/luv/Luv/Handle/#val-has_ref
	**/
	static public function hasRef(handle:Handle):Bool;

	/**
		Gets the size of the OS send buffer for a socket.

		@see https://aantron.github.io/luv/luv/Luv/Handle/#val-send_buffer_size
	**/
	static public function sendBufferSize(handle:SocketHandle):Result<Int>;

	/**
		Sets the size of the OS send buffer for a socket.

		@see https://aantron.github.io/luv/luv/Luv/Handle/#val-set_send_buffer_size
	**/
	static public function setSendBufferSize(handle:SocketHandle, size:Int):Result<Result.NoData>;

	/**
		Gets the size of the OS receive buffer for a socket.

		@see https://aantron.github.io/luv/luv/Luv/Handle/#val-recv_buffer_size
	**/
	static public function recvBufferSize(handle:SocketHandle):Result<Int>;

	/**
		Sets the size of the OS receive buffer for a socket.

		@see https://aantron.github.io/luv/luv/Luv/Handle/#val-set_recv_buffer_size
	**/
	static public function setRecvBufferSize(handle:SocketHandle, size:Int):Result<Result.NoData>;

// TODO
	// /**
	// 	Retrieves the file descriptor associated with the handle.

	// 	@see https://aantron.github.io/luv/luv/Luv/Handle/#val-fileno
	// **/
	// static public function fileno(handle:FileNo):Result<OsFd>;
}