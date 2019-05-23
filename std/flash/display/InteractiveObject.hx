package flash.display;

extern class InteractiveObject extends DisplayObject {
	@:flash.property var accessibilityImplementation(get,set) : flash.accessibility.AccessibilityImplementation;
	@:flash.property var contextMenu(get,set) : flash.ui.ContextMenu;
	@:flash.property var doubleClickEnabled(get,set) : Bool;
	@:flash.property var focusRect(get,set) : Dynamic;
	@:flash.property var mouseEnabled(get,set) : Bool;
	@:flash.property @:require(flash11) var needsSoftKeyboard(get,set) : Bool;
	@:flash.property @:require(flash11) var softKeyboardInputAreaOfInterest(get,set) : flash.geom.Rectangle;
	@:flash.property var tabEnabled(get,set) : Bool;
	@:flash.property var tabIndex(get,set) : Int;
	function new() : Void;
	private function get_accessibilityImplementation() : flash.accessibility.AccessibilityImplementation;
	private function get_contextMenu() : flash.ui.ContextMenu;
	private function get_doubleClickEnabled() : Bool;
	private function get_focusRect() : Dynamic;
	private function get_mouseEnabled() : Bool;
	private function get_needsSoftKeyboard() : Bool;
	private function get_softKeyboardInputAreaOfInterest() : flash.geom.Rectangle;
	private function get_tabEnabled() : Bool;
	private function get_tabIndex() : Int;
	@:require(flash11) function requestSoftKeyboard() : Bool;
	private function set_accessibilityImplementation(value : flash.accessibility.AccessibilityImplementation) : flash.accessibility.AccessibilityImplementation;
	private function set_contextMenu(value : flash.ui.ContextMenu) : flash.ui.ContextMenu;
	private function set_doubleClickEnabled(value : Bool) : Bool;
	private function set_focusRect(value : Dynamic) : Dynamic;
	private function set_mouseEnabled(value : Bool) : Bool;
	private function set_needsSoftKeyboard(value : Bool) : Bool;
	private function set_softKeyboardInputAreaOfInterest(value : flash.geom.Rectangle) : flash.geom.Rectangle;
	private function set_tabEnabled(value : Bool) : Bool;
	private function set_tabIndex(value : Int) : Int;
}
