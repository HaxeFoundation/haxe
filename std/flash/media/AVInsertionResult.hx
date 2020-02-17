package flash.media;

extern class AVInsertionResult extends AVResult {
	@:flash.property var insertedBeforeReadHead(get,never) : Bool;
	@:flash.property var periodIndex(get,never) : Int;
	function new(result : Int, periodIndex : Int, insertedBeforeReadHead : Bool) : Void;
	private function get_insertedBeforeReadHead() : Bool;
	private function get_periodIndex() : Int;
}
