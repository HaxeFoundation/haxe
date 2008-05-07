package flash.accessibility;

extern class AccessibilityImplementation {
	function new() : Void;
	function accDoDefaultAction(childID : UInt) : Void;
	function accLocation(childID : UInt) : Dynamic;
	function accSelect(operation : UInt, childID : UInt) : Void;
	var errno : UInt;
	function getChildIDArray() : Array<Dynamic>;
	function get_accDefaultAction(childID : UInt) : String;
	function get_accFocus() : UInt;
	function get_accName(childID : UInt) : String;
	function get_accRole(childID : UInt) : UInt;
	function get_accSelection() : Array<Dynamic>;
	function get_accState(childID : UInt) : UInt;
	function get_accValue(childID : UInt) : String;
	function isLabeledBy(labelBounds : flash.geom.Rectangle) : Bool;
	var stub : Bool;
}
