package flash.system;

@:native("flash.system.MessageChannelState") extern enum abstract MessageChannelState(String) {
	var CLOSED;
	var CLOSING;
	var OPEN;
}
