package byte;

abstract ByteData(haxe.io.Bytes) {

	public var length(get,never):Int;
	inline function get_length() return this.length;

	inline public function readByte(i:Int) return this.get(i);

	inline function new(data) {
		this = data;
	}

	inline static public function ofString(s:String):ByteData {
		return new ByteData(haxe.io.Bytes.ofString(s));
	}

	inline static public function ofBytes(b:haxe.io.Bytes):ByteData {
		return new ByteData(b);
	}

	inline public function readString(pos:Int, len:Int) {
		return this.getString(pos, len);
	}
}
