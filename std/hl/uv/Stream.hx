package hl.uv;

@:hlNative("uv")
class Stream extends Handle {

	public function write( bytes : haxe.io.Bytes, ?onWrite : Bool -> Void, pos = 0, len = -1 ) {
		if( len < 0 ) len = bytes.length - pos;
		if( pos < 0 || len < 0 || pos+len > bytes.length ) throw haxe.io.Error.OutsideBounds;
		if( handle == null || !stream_write(handle, (bytes : hl.Bytes).offset(pos), len, onWrite) ) throw new haxe.io.Eof();
	}

	public function readStartRaw( onData : hl.Bytes -> Int -> Void ) {
		if( handle == null || !stream_read_start(handle, onData) ) throw new haxe.io.Eof();
	}

	public function readStart( onData : haxe.io.Bytes -> Void ) {
		readStartRaw(function(b, len) onData(if( len < 0 ) null else b.toBytes(len)));
	}

	public function readStop() {
		if( handle != null ) stream_read_stop(handle);
	}

	public function listen( n : Int, onConnect : Void -> Void ) {
		if( handle == null || !stream_listen(handle, n, onConnect) ) throw new haxe.io.Eof();
	}

	// --

	static function stream_write( handle : HandleData, bytes: hl.Bytes, len : Int, callb : Bool -> Void ) : Bool {
		return false;
	}

	static function stream_read_start( handle : HandleData, callb : hl.Bytes -> Int -> Void ) {
		return false;
	}

	static function stream_read_stop( handle : HandleData ) {
	}

	static function stream_listen( handle : HandleData, n : Int, callb : Void -> Void ) {
		return false;
	}

}