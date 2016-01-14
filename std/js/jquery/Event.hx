/* This file is generated, do not edit! Visit http://api.jquery.com/ for API documentation. */
package js.jquery;
@:native("$.Event") extern class Event extends js.html.Event {
	/**
		An optional object of data passed to an event method when the current executing handler is bound.
	**/
	public var data : Dynamic;
	/**
		The element where the currently-called jQuery event handler was attached.
	**/
	public var delegateTarget : js.html.Element;
	/**
		Returns whether <a href="/event.preventDefault/">event.preventDefault()</a> was ever called on this event object.
	**/
	public function isDefaultPrevented():Bool;
	/**
		Returns whether event.stopImmediatePropagation() was ever called on this event object.
	**/
	public function isImmediatePropagationStopped():Bool;
	/**
		Returns whether <a href="/event.stopPropagation/">event.stopPropagation()</a> was ever called on this event object.
	**/
	public function isPropagationStopped():Bool;
	/**
		Indicates whether the META key was pressed when the event fired.
	**/
	public var metaKey : Bool;
	/**
		The namespace specified when the event was triggered.
	**/
	public var namespace : String;
	/**
		The mouse position relative to the left edge of the document.
	**/
	public var pageX : Float;
	/**
		The mouse position relative to the top edge of the document.
	**/
	public var pageY : Float;
	/**
		The other DOM element involved in the event, if any.
	**/
	public var relatedTarget : js.html.Element;
	/**
		The last value returned by an event handler that was triggered by this event, unless the value was <code>undefined</code>.
	**/
	public var result : Dynamic;
	/**
		For key or mouse events, this property indicates the specific key or button that was pressed.
	**/
	public var which : Float;
	public var altKey : Bool;
	public var button : Int;
	public var buttons : Int;
	public var char : Int;
	public var charCode : Int;
	public var clientX : Int;
	public var clientY : Int;
	public var ctrlKey : Bool;
	public var detail : Int;
	public var key : String;
	public var keyCode : Int;
	public var offsetX : Int;
	public var offsetY : Int;
	public var screenX : Int;
	public var screenY : Int;
	public var shiftKey : Bool;
	public var toElement : js.html.Element;
	public var view : js.html.Window;
}