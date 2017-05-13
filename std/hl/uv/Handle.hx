package hl.uv;

@:hlNative("uv")
class Handle {
	public var handle : HandleData;

	function new(h) {
		handle = h;
	}

	public function close( ?callb ) {
		if( handle != null ) close_handle(handle, callb);
		handle = null;
	}

	static function close_handle( h : HandleData, callb : Void->Void ) : Void {
	}

}
