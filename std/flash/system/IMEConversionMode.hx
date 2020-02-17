package flash.system;

@:native("flash.system.IMEConversionMode") extern enum abstract IMEConversionMode(String) {
	var ALPHANUMERIC_FULL;
	var ALPHANUMERIC_HALF;
	var CHINESE;
	var JAPANESE_HIRAGANA;
	var JAPANESE_KATAKANA_FULL;
	var JAPANESE_KATAKANA_HALF;
	var KOREAN;
	var UNKNOWN;
}
