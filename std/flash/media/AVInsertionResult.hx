package flash.media;

extern class AVInsertionResult extends AVResult {
	var insertedBeforeReadHead(default,null) : Bool;
	var periodIndex(default,null) : Int;
	function new(result : Int, periodIndex : Int, insertedBeforeReadHead : Bool) : Void;
}
