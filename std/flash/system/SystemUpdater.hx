package flash.system;

@:require(flash10_1) extern class SystemUpdater extends flash.events.EventDispatcher {
	function new() : Void;
	function cancel() : Void;
	function update(type : SystemUpdaterType) : Void;
}
