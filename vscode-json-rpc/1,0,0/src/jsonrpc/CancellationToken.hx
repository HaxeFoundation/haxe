package jsonrpc;

abstract CancellationToken(CancellationTokenImpl) {
	public var canceled(get, never):Bool;

	inline function get_canceled()
		return this.canceled;

	public inline function setCallback(cb:Void->Void)
		this.callback = cb;
}

abstract CancellationTokenSource(CancellationTokenImpl) {
	public var token(get, never):CancellationToken;

	inline function get_token():CancellationToken
		return cast this;

	public inline function new()
		this = new CancellationTokenImpl();

	public inline function cancel()
		this.cancel();
}

private class CancellationTokenImpl {
	public var canceled(default, null):Bool;
	public var callback:Void->Void;

	public inline function new() {
		canceled = false;
	}

	public inline function cancel() {
		if (canceled)
			return;
		canceled = true;
		if (callback != null)
			callback();
	}
}
