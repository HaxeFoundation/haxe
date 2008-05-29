package flash.text.engine;

extern class GroupElement extends flash.text.engine.ContentElement {
	function new(?elements : flash.Vector<flash.text.engine.ContentElement>, ?elementFormat : flash.text.engine.ElementFormat, ?eventMirror : flash.events.EventDispatcher, ?textRotation : String) : Void;
	var elementCount(default,null) : Int;
	function getElementAt(index : Int) : flash.text.engine.ContentElement;
	function getElementAtCharIndex(charIndex : Int) : flash.text.engine.ContentElement;
	function getElementIndex(element : flash.text.engine.ContentElement) : Int;
	function groupElements(beginIndex : Int, endIndex : Int) : flash.text.engine.GroupElement;
	function mergeTextElements(beginIndex : Int, endIndex : Int) : flash.text.engine.TextElement;
	function replaceElements(beginIndex : Int, endIndex : Int, newElements : flash.Vector<flash.text.engine.ContentElement>) : flash.Vector<flash.text.engine.ContentElement>;
	function setElements(value : flash.Vector<flash.text.engine.ContentElement>) : Void;
	function splitTextElement(elementIndex : Int, splitIndex : Int) : flash.text.engine.TextElement;
	function ungroupElements(groupIndex : Int) : Void;
}
