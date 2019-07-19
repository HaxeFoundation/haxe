package haxe.io;

@:forward(length, get, getDouble, getFloat, getInt32, getInt64, getString, getUInt16, sub, toHex, toString)
abstract BytesView(Bytes) from Bytes {
	var bytes(get,never):Bytes;

	public inline function compare(other:BytesView):Int
		return this.compare(other.bytes);

	/**
		Create a copy of this content.
	**/
	public function toBytes():Bytes {
		var result = Bytes.alloc(this.length);
		for (i in 0...this.length) {
			result.set(i, this.get(i));
		}
		return result;
	}

	inline function get_bytes():Bytes
		return this;
}