package haxe.io;

/**
	Represents a relative or absolute file path.
**/
abstract FilePath(String) from String {
	@:from public static function encode(bytes:Bytes):FilePath {
		// TODO: standard UTF-8 decoding, except any invalid bytes is replaced
		// with (for example) U+FFFD, followed by the byte itself as a codepoint
		return null;
	}

	public function decode():Bytes {
		return null;
	}

	/**
		The components of `this` path.
	**/
	public var components(get, never):Array<FilePath>;

	private function get_components():Array<FilePath> {
		return this.split("/");
	}

	@:op(A / B)
	public function addComponent(other:FilePath):FilePath {
		return this + "/" + other.get_raw();
	}

	private function get_raw():String
		return this;
}
