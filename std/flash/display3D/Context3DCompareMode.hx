package flash.display3D;

@:native("flash.display3D.Context3DCompareMode") extern enum abstract Context3DCompareMode(String) {
	var ALWAYS;
	var EQUAL;
	var GREATER;
	var GREATER_EQUAL;
	var LESS;
	var LESS_EQUAL;
	var NEVER;
	var NOT_EQUAL;
}
