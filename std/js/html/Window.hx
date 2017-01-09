/*
 * Copyright (C)2005-2017 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

// This file is generated from mozilla\Window.webidl. Do not edit!

package js.html;

/**
	The `window` object represents a window containing a DOM document; the `document` property points to the DOM document loaded in that window.

	Documentation [Window](https://developer.mozilla.org/en-US/docs/Web/API/Window) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/Window$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/Window>
**/
@:native("Window")
extern class Window extends EventTarget
{
	
	/**
		Returns a reference to the current window.
	**/
	var window(default,null) : Window;
	
	/**
		Returns an object reference to the window object itself.
	**/
	var self(default,null) : Window;
	
	/**
		Returns a reference to the document that the window contains.
	**/
	var document(default,null) : HTMLDocument;
	
	/**
		Gets/sets the name of the window.
	**/
	var name : String;
	
	/**
		Gets/sets the location, or current URL, of the window object.
	**/
	var location(default,null) : Location;
	
	/**
		Returns a reference to the history object.
	**/
	var history(default,null) : History;
	
	/**
		Returns the locationbar object, whose visibility can be toggled in the window.
	**/
	var locationbar(default,null) : BarProp;
	
	/**
		Returns the menubar object, whose visibility can be toggled in the window.
	**/
	var menubar(default,null) : BarProp;
	
	/**
		Returns the personalbar object, whose visibility can be toggled in the window.
	**/
	var personalbar(default,null) : BarProp;
	
	/**
		Returns the scrollbars object, whose visibility can be toggled in the window.
	**/
	var scrollbars(default,null) : BarProp;
	
	/**
		Returns the statusbar object, whose visibility can be toggled in the window.
	**/
	var statusbar(default,null) : BarProp;
	
	/**
		Returns the toolbar object, whose visibility can be toggled in the window.
	**/
	var toolbar(default,null) : BarProp;
	
	/**
		Gets/sets the text in the statusbar at the bottom of the browser.
	**/
	var status : String;
	
	/**
		This property indicates whether the current window is closed or not.
	**/
	var closed(default,null) : Bool;
	
	/**
		Returns an array of the subframes in the current window.
	**/
	var frames(default,null) : Window;
	
	/**
		Returns the number of frames in the window. See also `window.frames`.
	**/
	var length(default,null) : Int;
	
	/**
		Returns a reference to the topmost window in the window hierarchy. This property is read only.
	**/
	var top(default,null) : Window;
	
	/**
		Returns a reference to the window that opened this current window.
	**/
	var opener : Dynamic;
	
	/**
		Returns a reference to the parent of the current window or subframe.
	**/
	var parent(default,null) : Window;
	
	/**
		Returns the element in which the window is embedded, or null if the window is not embedded.
	**/
	var frameElement(default,null) : Element;
	
	/**
		Returns a reference to the navigator object.
	**/
	var navigator(default,null) : Navigator;
	
	/**
		Returns the orientation in degrees (in 90 degree increments) of the viewport relative to the device's natural orientation.
	**/
	var orientation(default,null) : Int;
	var onorientationchange : haxe.Constraints.Function;
	
	/**
		Returns a reference to the screen object associated with the window.
	**/
	var screen(default,null) : Screen;
	
	/**
		Gets the width of the content area of the browser window including, if rendered, the vertical scrollbar.
	**/
	var innerWidth : Dynamic;
	
	/**
		Gets the height of the content area of the browser window including, if rendered, the horizontal scrollbar.
	**/
	var innerHeight : Dynamic;
	
	/**
		Returns the number of pixels that the document has already been scrolled horizontally.
	**/
	var scrollX(default,null) : Int;
	var pageXOffset(default,null) : Int;
	
	/**
		Returns the number of pixels that the document has already been scrolled vertically.
	**/
	var scrollY(default,null) : Int;
	var pageYOffset(default,null) : Int;
	
	/**
		Returns the horizontal distance of the left border of the user's browser from the left side of the screen.
	**/
	var screenX : Dynamic;
	
	/**
		Returns the vertical distance of the top border of the user's browser from the top side of the screen.
	**/
	var screenY : Dynamic;
	
	/**
		Gets the width of the outside of the browser window.
	**/
	var outerWidth : Dynamic;
	
	/**
		Gets the height of the outside of the browser window.
	**/
	var outerHeight : Dynamic;
	
	/**
		Provides a hosting area for performance related attributes.
	**/
	var performance(default,null) : Performance;
	
	/**
		Returns the ratio between physical pixels and device independent pixels in the current display.
	**/
	var devicePixelRatio(default,null) : Float;
	
	/**
		The maximum offset that the window can be scrolled to horizontally, that is the document width minus the viewport width.
	**/
	var scrollMaxX(default,null) : Int;
	
	/**
		The maximum offset that the window can be scrolled to vertically (i.e., the document height minus the viewport height).
	**/
	var scrollMaxY(default,null) : Int;
	
	/**
		This property indicates whether the window is displayed in full screen or not.
	**/
	var fullScreen : Bool;
	var onwheel : haxe.Constraints.Function;
	var ondevicemotion : haxe.Constraints.Function;
	var ondeviceorientation : haxe.Constraints.Function;
	var onabsolutedeviceorientation : haxe.Constraints.Function;
	var ondeviceproximity : haxe.Constraints.Function;
	var onuserproximity : haxe.Constraints.Function;
	var ondevicelight : haxe.Constraints.Function;
	
	/**
		Returns a reference to the content element in the current window. The obsolete variant with underscore is no longer available from Web content.
	**/
	var content(default,null) : Dynamic;
	
	/**
		Returns a reference to the console object which provides access to the browser's debugging console.
	**/
	var console(default,null) : Console;
	
	/**
		Returns the browser crypto object.
	**/
	var crypto(default,null) : Crypto;
	var onabort : haxe.Constraints.Function;
	var onblur : haxe.Constraints.Function;
	var onfocus : haxe.Constraints.Function;
	var oncanplay : haxe.Constraints.Function;
	var oncanplaythrough : haxe.Constraints.Function;
	var onchange : haxe.Constraints.Function;
	var onclick : haxe.Constraints.Function;
	var oncontextmenu : haxe.Constraints.Function;
	var ondblclick : haxe.Constraints.Function;
	var ondrag : haxe.Constraints.Function;
	var ondragend : haxe.Constraints.Function;
	var ondragenter : haxe.Constraints.Function;
	var ondragleave : haxe.Constraints.Function;
	var ondragover : haxe.Constraints.Function;
	var ondragstart : haxe.Constraints.Function;
	var ondrop : haxe.Constraints.Function;
	var ondurationchange : haxe.Constraints.Function;
	var onemptied : haxe.Constraints.Function;
	var onended : haxe.Constraints.Function;
	var oninput : haxe.Constraints.Function;
	var oninvalid : haxe.Constraints.Function;
	var onkeydown : haxe.Constraints.Function;
	var onkeypress : haxe.Constraints.Function;
	var onkeyup : haxe.Constraints.Function;
	var onload : haxe.Constraints.Function;
	var onloadeddata : haxe.Constraints.Function;
	var onloadedmetadata : haxe.Constraints.Function;
	var onloadstart : haxe.Constraints.Function;
	var onmousedown : haxe.Constraints.Function;
	var onmouseenter : haxe.Constraints.Function;
	var onmouseleave : haxe.Constraints.Function;
	var onmousemove : haxe.Constraints.Function;
	var onmouseout : haxe.Constraints.Function;
	var onmouseover : haxe.Constraints.Function;
	var onmouseup : haxe.Constraints.Function;
	var onpause : haxe.Constraints.Function;
	var onplay : haxe.Constraints.Function;
	var onplaying : haxe.Constraints.Function;
	var onprogress : haxe.Constraints.Function;
	var onratechange : haxe.Constraints.Function;
	var onreset : haxe.Constraints.Function;
	var onresize : haxe.Constraints.Function;
	var onscroll : haxe.Constraints.Function;
	var onseeked : haxe.Constraints.Function;
	var onseeking : haxe.Constraints.Function;
	var onselect : haxe.Constraints.Function;
	var onshow : haxe.Constraints.Function;
	var onstalled : haxe.Constraints.Function;
	var onsubmit : haxe.Constraints.Function;
	var onsuspend : haxe.Constraints.Function;
	var ontimeupdate : haxe.Constraints.Function;
	var onvolumechange : haxe.Constraints.Function;
	var onwaiting : haxe.Constraints.Function;
	var onpointercancel : haxe.Constraints.Function;
	var onpointerdown : haxe.Constraints.Function;
	var onpointerup : haxe.Constraints.Function;
	var onpointermove : haxe.Constraints.Function;
	var onpointerout : haxe.Constraints.Function;
	var onpointerover : haxe.Constraints.Function;
	var onpointerenter : haxe.Constraints.Function;
	var onpointerleave : haxe.Constraints.Function;
	var ongotpointercapture : haxe.Constraints.Function;
	var onlostpointercapture : haxe.Constraints.Function;
	var onfullscreenchange : haxe.Constraints.Function;
	var onfullscreenerror : haxe.Constraints.Function;
	var onpointerlockchange : haxe.Constraints.Function;
	var onpointerlockerror : haxe.Constraints.Function;
	var indexedDB(default,null) : js.html.idb.Factory;
	var onerror : haxe.extern.EitherType<Event,String> -> String -> Int -> Int -> Dynamic -> Bool;
	
	/**
		Returns a `SpeechSynthesis` object, which is the entry point into using Web Speech API speech synthesis functionality.
	**/
	var speechSynthesis(default,null) : SpeechSynthesis;
	var ontouchstart : haxe.Constraints.Function;
	var ontouchend : haxe.Constraints.Function;
	var ontouchmove : haxe.Constraints.Function;
	var ontouchcancel : haxe.Constraints.Function;
	var onafterprint : haxe.Constraints.Function;
	var onbeforeprint : haxe.Constraints.Function;
	var onbeforeunload : Event -> String;
	var onhashchange : haxe.Constraints.Function;
	var onlanguagechange : haxe.Constraints.Function;
	var onmessage : haxe.Constraints.Function;
	var onoffline : haxe.Constraints.Function;
	var ononline : haxe.Constraints.Function;
	var onpagehide : haxe.Constraints.Function;
	var onpageshow : haxe.Constraints.Function;
	var onpopstate : haxe.Constraints.Function;
	var onstorage : haxe.Constraints.Function;
	var onunload : haxe.Constraints.Function;
	
	/**
		Returns a reference to the local storage object used to store data that may only be accessed by the origin that created it.
	**/
	var localStorage(default,null) : Storage;
	
	/**
		Returns a storage object for storing data within a single page session.
	**/
	var sessionStorage(default,null) : Storage;
	
	/** @throws DOMError */
	
	/**
		Closes the current window.
	**/
	function close() : Void;
	/** @throws DOMError */
	
	/**
		This method stops window loading.
	**/
	function stop() : Void;
	/** @throws DOMError */
	
	/**
		Sets focus on the current window.
	**/
	function focus() : Void;
	/** @throws DOMError */
	
	/**
		Sets focus away from the window.
	**/
	function blur() : Void;
	/** @throws DOMError */
	
	/**
		Opens a new window.
	**/
	function open( ?url : String = "", ?target : String = "", ?features : String = "" ) : Window;
	/** @throws DOMError */
	@:overload( function() : Void {} )
	
	/**
		Displays an alert dialog.
	**/
	function alert( message : String ) : Void;
	/** @throws DOMError */
	
	/**
		Displays a dialog with a message that the user needs to respond to.
	**/
	function confirm( ?message : String = "" ) : Bool;
	/** @throws DOMError */
	
	/**
		Returns the text entered by the user in a prompt dialog.
	**/
	function prompt( ?message : String = "", ?default_ : String = "" ) : String;
	/** @throws DOMError */
	
	/**
		Opens the Print Dialog to print the current document.
	**/
	function print() : Void;
	/** @throws DOMError */
	
	/**
		Provides a secure means for one window to send a string of data to another window, which need not be within the same domain as the first.
	**/
	function postMessage( message : Dynamic, targetOrigin : String, ?transfer : Array<Dynamic> ) : Void;
	
	/**
		Registers the window to capture all events of the specified type.
	**/
	function captureEvents() : Void;
	
	/**
		Releases the window from trapping events of a specific type.
	**/
	function releaseEvents() : Void;
	/** @throws DOMError */
	
	/**
		Returns the selection object representing the selected item(s).
	**/
	function getSelection() : Selection;
	/** @throws DOMError */
	
	/**
		Gets computed style for the specified element. Computed style indicates the computed values of all CSS properties of the element.
	**/
	function getComputedStyle( elt : Element, ?pseudoElt : String = "" ) : CSSStyleDeclaration;
	/** @throws DOMError */
	
	/**
		Returns a `MediaQueryList` object representing the specified media query string.
	**/
	function matchMedia( query : String ) : MediaQueryList;
	/** @throws DOMError */
	
	/**
		Moves the window to the specified coordinates.
	**/
	function moveTo( x : Int, y : Int ) : Void;
	/** @throws DOMError */
	
	/**
		Moves the current window by a specified amount.
	**/
	function moveBy( x : Int, y : Int ) : Void;
	/** @throws DOMError */
	
	/**
		Dynamically resizes window.
	**/
	function resizeTo( x : Int, y : Int ) : Void;
	/** @throws DOMError */
	
	/**
		Resizes the current window by a certain amount.
	**/
	function resizeBy( x : Int, y : Int ) : Void;
	@:overload( function( x : Float, y : Float ) : Void {} )
	
	/**
		Scrolls the window to a particular place in the document.
	**/
	function scroll( ?options : ScrollToOptions ) : Void;
	@:overload( function( x : Float, y : Float ) : Void {} )
	
	/**
		Scrolls to a particular set of coordinates in the document.
	**/
	function scrollTo( ?options : ScrollToOptions ) : Void;
	@:overload( function( x : Float, y : Float ) : Void {} )
	
	/**
		Scrolls the document in the window by the given amount.
	**/
	function scrollBy( ?options : ScrollToOptions ) : Void;
	/** @throws DOMError */
	function requestAnimationFrame( callback : Float -> Void ) : Int;
	/** @throws DOMError */
	function cancelAnimationFrame( handle : Int ) : Void;
	/** @throws DOMError */
	
	/**
		Gets default computed style for the specified element, ignoring author stylesheets.
	**/
	function getDefaultComputedStyle( elt : Element, ?pseudoElt : String = "" ) : CSSStyleDeclaration;
	
	/**
		Scrolls the document by the given number of lines.
	**/
	function scrollByLines( numLines : Int, ?options : ScrollOptions ) : Void;
	
	/**
		Scrolls the current document by the specified number of pages.
	**/
	function scrollByPages( numPages : Int, ?options : ScrollOptions ) : Void;
	/** @throws DOMError */
	
	/**
		Sizes the window according to its content.
	**/
	function sizeToContent() : Void;
	
	/**
		Updates the state of commands of the current chrome window (UI).
	**/
	function updateCommands( action : String, ?sel : Selection, ?reason : Int = 0 ) : Void;
	/** @throws DOMError */
	
	/**
		Searches for a given string in a window.
	**/
	function find( ?str : String = "", ?caseSensitive : Bool = false, ?backwards : Bool = false, ?wrapAround : Bool = false, ?wholeWord : Bool = false, ?searchInFrames : Bool = false, ?showDialog : Bool = false ) : Bool;
	
	/**
		Writes a message to the console.
	**/
	function dump( str : String ) : Void;
	
	/**
		Toggles a user's ability to resize a window.
	**/
	function setResizable( resizable : Bool ) : Void;
	/** @throws DOMError */
	function fetch( input : haxe.extern.EitherType<Request,String>, ?init : RequestInit ) : Promise<Response>;
	/** @throws DOMError */
	@:overload( function( aImage : haxe.extern.EitherType<ImageElement,haxe.extern.EitherType<VideoElement,haxe.extern.EitherType<CanvasElement,haxe.extern.EitherType<Blob,haxe.extern.EitherType<ImageData,haxe.extern.EitherType<CanvasRenderingContext2D,ImageBitmap>>>>>> ) : Promise<ImageBitmap> {} )
	function createImageBitmap( aImage : haxe.extern.EitherType<ImageElement,haxe.extern.EitherType<VideoElement,haxe.extern.EitherType<CanvasElement,haxe.extern.EitherType<Blob,haxe.extern.EitherType<ImageData,haxe.extern.EitherType<CanvasRenderingContext2D,ImageBitmap>>>>>>, aSx : Int, aSy : Int, aSw : Int, aSh : Int ) : Promise<ImageBitmap>;
	/** @throws DOMError */
	function btoa( btoa : String ) : String;
	/** @throws DOMError */
	function atob( atob : String ) : String;
	/** @throws DOMError */
	@:overload( function( handler : haxe.Constraints.Function, ?timeout : Int = 0, arguments : haxe.extern.Rest<Dynamic> ) : Int {} )
	function setTimeout( handler : String, ?timeout : Int = 0, unused : haxe.extern.Rest<Dynamic> ) : Int;
	function clearTimeout( ?handle : Int = 0 ) : Void;
	/** @throws DOMError */
	@:overload( function( handler : haxe.Constraints.Function, ?timeout : Int, arguments : haxe.extern.Rest<Dynamic> ) : Int {} )
	function setInterval( handler : String, ?timeout : Int, unused : haxe.extern.Rest<Dynamic> ) : Int;
	function clearInterval( ?handle : Int = 0 ) : Void;
}