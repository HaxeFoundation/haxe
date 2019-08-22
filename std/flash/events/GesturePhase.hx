package flash.events;

@:native("flash.events.GesturePhase") @:require(flash10_1) extern enum abstract GesturePhase(String) {
	var ALL;
	var BEGIN;
	var END;
	var UPDATE;
}
