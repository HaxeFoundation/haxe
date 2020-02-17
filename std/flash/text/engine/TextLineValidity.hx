package flash.text.engine;

@:native("flash.text.engine.TextLineValidity") extern enum abstract TextLineValidity(String) {
	var INVALID;
	var POSSIBLY_INVALID;
	var STATIC;
	var VALID;
}
