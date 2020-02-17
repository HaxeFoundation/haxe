package flash.filters;

@:native("flash.filters.DisplacementMapFilterMode") extern enum abstract DisplacementMapFilterMode(String) {
	var CLAMP;
	var COLOR;
	var IGNORE;
	var WRAP;
}
