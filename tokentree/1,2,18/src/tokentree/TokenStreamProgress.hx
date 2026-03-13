package tokentree;

class TokenStreamProgress {
	var stream:TokenStream;
	var pos:Int;

	public function new(stream:TokenStream) {
		this.stream = stream;
		pos = -1;
	}

	public function streamHasChanged():Bool {
		if (pos == -1) {
			pos = stream.getStreamIndex();
			return true;
		}
		var oldPos:Int = pos;
		pos = stream.getStreamIndex();
		return (pos != oldPos);
	}
}