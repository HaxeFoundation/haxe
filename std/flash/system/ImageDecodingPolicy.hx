package flash.system;

@:native("flash.system.ImageDecodingPolicy") extern enum abstract ImageDecodingPolicy(String) {
	var ON_DEMAND;
	var ON_LOAD;
}
