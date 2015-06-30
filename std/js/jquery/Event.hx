/* This file is generated, do not edit! Visit http://api.jquery.com/ for API documentation. */
package js.jquery;
@:final @:native("$.Event") extern class Event extends js.html.Event {
	/**
		The element where the currently-called jQuery event handler was attached.
	**/
	public var delegateTarget : js.html.Element;
	/**
		The namespace specified when the event was triggered.
	**/
	public var namespace : String;
	/**
		For key or mouse events, this property indicates the specific key or button that was pressed.
	**/
	public var which : Float;
	/**
		  Returns whether <a href="/event.stopPropagation/">event.stopPropagation()</a> was ever called on this event object. 
	**/
	public function isPropagationStopped():Bool;
	/**
		  Returns whether event.stopImmediatePropagation() was ever called on this event object. 
	**/
	public function isImmediatePropagationStopped():Bool;
	/**
		An optional object of data passed to an event method when the current executing handler is bound.  
	**/
	public var data : Dynamic;
	/**
		Indicates whether the META key was pressed when the event fired.
	**/
	public var metaKey : Bool;
	/**
		The other DOM element involved in the event, if any.
	**/
	public var relatedTarget : js.html.Element;
	/**
		The mouse position relative to the left edge of the document.
	**/
	public var pageX : Float;
	/**
		The last value returned by an event handler that was triggered by this event, unless the value was <code>undefined</code>.
	**/
	public var result : Dynamic;
	/**
		Returns whether <a href="/event.preventDefault/">event.preventDefault()</a> was ever called on this event object. 
	**/
	public function isDefaultPrevented():Bool;
	/**
		The mouse position relative to the top edge of the document.
	**/
	public var pageY : Float;
}