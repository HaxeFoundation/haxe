package haxe.io;

class StreamTools {
	/**
		Creates a pipeline out of the given streams. `input` is piped to the first
		element in `intermediate`, which is piped to the next element in
		`intermediate`, and so on, until the last stream is piped to `output`. If
		`intermediate` is `null`, it is treated as an empty array and `input` is
		connected directly to `output`.
	**/
	public static function pipeline(input:IReadable, ?intermediate:Array<IDuplex>, output:IWritable):Void {
		if (intermediate == null || intermediate.length == 0)
			return input.pipe(output);

		input.pipe(intermediate[0]);
		for (i in 0...intermediate.length - 1) {
			intermediate[i].pipe(intermediate[i + 1]);
		}
		intermediate[intermediate.length - 1].pipe(output);
	}
}
