package flash.media;

extern class AVInsertionResult extends AVResult {
	var insertedBeforeReadHead(get,never) : Bool;
	var periodIndex(get,never) : Int;
	function new(result : Int, periodIndex : Int, insertedBeforeReadHead : Bool) : Void;
	private function get_insertedBeforeReadHead() : Bool;
	private function get_periodIndex() : Int;
}
