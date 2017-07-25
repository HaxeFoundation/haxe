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
	/**
		
				Returns a `Boolean` that is `true` if the Alt ( Option or ⌥ on OS X) key was active when the key event was generated.
			
	**/
	public var altKey : Bool;
	/**
		
				The button number that was pressed when the mouse event was fired. 
			
	**/
	public var button : Int;
	/**
		
				
				 The buttons being pressed when the mouse event was fired
				 
			
	**/
	public var buttons : Int;
	public var char : Int;
	/**
		
				Returns a `Number` representing the Unicode reference number of the key; this attribute is used only by the `keypress` event. For keys whose `char` attribute contains multiple characters, this is the Unicode value of the first character in that attribute. In Firefox 26 this returns codes for printable characters.
				 Warning: This attribute is deprecated; you should use `KeyboardEvent.key` instead, if available.
				 
			
	**/
	public var charCode : Int;
	/**
		
				The X coordinate of the mouse pointer in local (DOM content) coordinates.
			
	**/
	public var clientX : Int;
	/**
		
				The Y coordinate of the mouse pointer in local (DOM content) coordinates.
			
	**/
	public var clientY : Int;
	/**
		
				Returns a `Boolean` that is `true` if the Ctrl key was active when the key event was generated.
			
	**/
	public var ctrlKey : Bool;
	/**
		
				Returns a `long` with details about the event, depending on the event type.
			
	**/
	public var detail : Int;
	/**
		
				Returns a `DOMString` representing the key value of the key represented by the event.
			
	**/
	public var key : String;
	/**
		
				Returns a `Number` representing a system and implementation dependent numerical code identifying the unmodified value of the pressed key.
				 Warning: This attribute is deprecated; you should use `KeyboardEvent.key` instead, if available.
				 
			
	**/
	public var keyCode : Int;
	public var offsetX : Int;
	public var offsetY : Int;
	/**
		
				The X coordinate of the mouse pointer in global (screen) coordinates.
			
	**/
	public var screenX : Int;
	/**
		
				The Y coordinate of the mouse pointer in global (screen) coordinates.
			
	**/
	public var screenY : Int;
	/**
		
				Returns a `Boolean` that is `true` if the Shift key was active when the key event was generated.
			
	**/
	public var shiftKey : Bool;
	public var toElement : js.html.Element;
	/**
		
				Returns a `WindowProxy` that contains the view that generated the event.
			
	**/
	public var view : js.html.Window;
	/**
		A convenient method of getting `$(this)`, which is typically the same as `$(evt.currentTarget)`.
		For detail, refer to https://api.jquery.com/event.currenttarget/.
	**/
	inline public function getThis():js.jquery.JQuery return new js.jquery.JQuery(js.Lib.nativeThis);
}