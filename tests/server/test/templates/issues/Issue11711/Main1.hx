class Main {
	static function main() {
		var jit = {align: {ptr: 42}};
		var bar =
		var excPos = jit.align.ptr * 5 + 8;
		trace(excPos);
		$type(excPos);
	}
}
