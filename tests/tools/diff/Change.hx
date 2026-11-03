package diff;

@:structInit
class Change {
	@:optional
	public var next:Null<Change>;
	public var inserted:Int;
	public var deleted:Int;
	public var line0:Int;
	public var line1:Int;

	public function toString() {
		return '[CHANGE inserted: $inserted, deleted: $deleted, line0: $line0, line1: $line1]';
	}
}
