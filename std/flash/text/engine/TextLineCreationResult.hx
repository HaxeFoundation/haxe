package flash.text.engine;

@:native("flash.text.engine.TextLineCreationResult") extern enum abstract TextLineCreationResult(String) {
	var COMPLETE;
	var EMERGENCY;
	var INSUFFICIENT_WIDTH;
	var SUCCESS;
}
