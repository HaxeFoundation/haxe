class StreamOutput {
	static function main() {
		// Output something immediately, then block until stdin has a byte.
		// A test can read this first trace line to confirm streaming works,
		// then send a byte to let the program continue.
		trace("before_stdin");
		Sys.stdin().readByte();
		trace("after_stdin");
	}
}
