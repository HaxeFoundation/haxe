package flash.automation;

@:require(flash10_1) extern class ActionGenerator {
	function new() : Void;
	function generateAction(action : AutomationAction) : Void;
	function generateActions(a : Array<Dynamic>) : Void;
}
