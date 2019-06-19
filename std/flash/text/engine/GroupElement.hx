package flash.text.engine;

extern final class GroupElement extends ContentElement {
	@:flash.property var elementCount(get,never) : Int;
	function new(?elements : flash.Vector<ContentElement>, ?elementFormat : ElementFormat, ?eventMirror : flash.events.EventDispatcher, ?textRotation : TextRotation) : Void;
	function getElementAt(index : Int) : ContentElement;
	function getElementAtCharIndex(charIndex : Int) : ContentElement;
	function getElementIndex(element : ContentElement) : Int;
	private function get_elementCount() : Int;
	function groupElements(beginIndex : Int, endIndex : Int) : GroupElement;
	function mergeTextElements(beginIndex : Int, endIndex : Int) : TextElement;
	function replaceElements(beginIndex : Int, endIndex : Int, newElements : flash.Vector<ContentElement>) : flash.Vector<ContentElement>;
	function setElements(value : flash.Vector<ContentElement>) : Void;
	function splitTextElement(elementIndex : Int, splitIndex : Int) : TextElement;
	function ungroupElements(groupIndex : Int) : Void;
}
