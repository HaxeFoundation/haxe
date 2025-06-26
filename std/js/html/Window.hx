/*
 * Copyright (C)2005-2019 Haxe Foundation
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

package js.html;

import js.lib.Promise;

/**
	The `Window` interface represents a window containing a DOM document; the `document` property points to the DOM document loaded in that window.

	Documentation [Window](https://developer.mozilla.org/en-US/docs/Web/API/Window) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/Window$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/Window>
**/
@:native("Window")
extern class Window extends EventTarget {

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
		Returns the current event, which is the event currently being handled by the JavaScript code's context, or `undefined` if no event is currently being handled. The `Event` object passed directly to event handlers should be used instead whenever possible.
	**/
	var event(default,null) : Dynamic;

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
		Returns a reference to the console object which provides access to the browser's debugging console.
	**/
	var console(default,null) : ConsoleInstance;

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
	var scrollX(default,null) : Float;

	/**
		An alias for `window.scrollX`.
	**/
	var pageXOffset(default,null) : Float;

	/**
		Returns the number of pixels that the document has already been scrolled vertically.
	**/
	var scrollY(default,null) : Float;

	/**
		An alias for `window.scrollY`
	**/
	var pageYOffset(default,null) : Float;

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
		Returns a `Performance` object, which includes the `Performance.timing` and `Performance.navigation` attributes, each of which is an object providing performance-related data. See also Using Navigation Timing for additional information and examples.
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
	var ondevicemotion : haxe.Constraints.Function;
	var ondeviceorientation : haxe.Constraints.Function;
	var onabsolutedeviceorientation : haxe.Constraints.Function;
	var ondeviceproximity : haxe.Constraints.Function;
	var onuserproximity : haxe.Constraints.Function;
	var ondevicelight : haxe.Constraints.Function;

	/**
		Returns a reference to the content element in the current window. Since Firefox 57 (initially Nightly-only), both versions are only available from chrome (privileged) code, and not available to the web anymore.
	**/
	var content(default,null) : Dynamic;

	/**
		Returns the orientation in degrees (in 90 degree increments) of the viewport relative to the device's natural orientation.
	**/
	var orientation(default,null) : Int;
	var onorientationchange : haxe.Constraints.Function;

	/**
		Returns the browser crypto object.
	**/
	var crypto(default,null) : Crypto;
	var onabort : haxe.Constraints.Function;
	var onblur : haxe.Constraints.Function;
	var onfocus : haxe.Constraints.Function;
	var onauxclick : haxe.Constraints.Function;
	var oncanplay : haxe.Constraints.Function;
	var oncanplaythrough : haxe.Constraints.Function;
	var onchange : haxe.Constraints.Function;
	var onclick : haxe.Constraints.Function;
	var onclose : haxe.Constraints.Function;
	var oncontextmenu : haxe.Constraints.Function;
	var ondblclick : haxe.Constraints.Function;
	var ondrag : haxe.Constraints.Function;
	var ondragend : haxe.Constraints.Function;
	var ondragenter : haxe.Constraints.Function;
	var ondragexit : haxe.Constraints.Function;
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
	var onloadend : haxe.Constraints.Function;
	var onloadstart : haxe.Constraints.Function;
	var onmousedown : haxe.Constraints.Function;
	var onmouseenter : haxe.Constraints.Function;
	var onmouseleave : haxe.Constraints.Function;
	var onmousemove : haxe.Constraints.Function;
	var onmouseout : haxe.Constraints.Function;
	var onmouseover : haxe.Constraints.Function;
	var onmouseup : haxe.Constraints.Function;
	var onwheel : haxe.Constraints.Function;
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
	var onselectstart : haxe.Constraints.Function;
	var ontoggle : haxe.Constraints.Function;
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
	var onanimationcancel : haxe.Constraints.Function;
	var onanimationend : haxe.Constraints.Function;
	var onanimationiteration : haxe.Constraints.Function;
	var onanimationstart : haxe.Constraints.Function;
	var ontransitioncancel : haxe.Constraints.Function;
	var ontransitionend : haxe.Constraints.Function;
	var ontransitionrun : haxe.Constraints.Function;
	var ontransitionstart : haxe.Constraints.Function;
	var onwebkitanimationend : haxe.Constraints.Function;
	var onwebkitanimationiteration : haxe.Constraints.Function;
	var onwebkitanimationstart : haxe.Constraints.Function;
	var onwebkittransitionend : haxe.Constraints.Function;
	var onerror : haxe.extern.EitherType<Event,String> -> String -> Int -> Int -> Dynamic -> Dynamic;

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
	var onbeforeunload : Event -> Null<String>;
	var onhashchange : haxe.Constraints.Function;
	var onlanguagechange : haxe.Constraints.Function;
	var onmessage : haxe.Constraints.Function;
	var onmessageerror : haxe.Constraints.Function;
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
	var origin(default,null) : String;

	/**
		Indicates whether a context is capable of using features that require secure contexts.
	**/
	var isSecureContext(default,null) : Bool;
	var indexedDB(default,null) : js.html.idb.Factory;
	var caches(default,null) : CacheStorage;

	/**
		Returns a reference to the session storage object used to store data that may only be accessed by the origin that created it.
	**/
	var sessionStorage(default,null) : Storage;


	/**
		Closes the current window.
		@throws DOMError
	**/
	function close() : Void;

	/**
		This method stops window loading.
		@throws DOMError
	**/
	function stop() : Void;

	/**
		Sets focus on the current window.
		@throws DOMError
	**/
	function focus() : Void;

	/**
		Sets focus away from the window.
		@throws DOMError
	**/
	function blur() : Void;

	/**
		Opens a new window.
		@throws DOMError
	**/
	function open( url : String = "", target : String = "", features : String = "" ) : Window;

	/**
		Displays an alert dialog.
		@throws DOMError
	**/
	@:overload( function() : Void {} )
	function alert( message : String ) : Void;

	/**
		Displays a dialog with a message that the user needs to respond to.
		@throws DOMError
	**/
	function confirm( message : String = "" ) : Bool;

	/**
		Returns the text entered by the user in a prompt dialog.
		@throws DOMError
	**/
	function prompt( message : String = "", default_ : String = "" ) : String;

	/**
		Opens the Print Dialog to print the current document.
		@throws DOMError
	**/
	function print() : Void;

	/**
		Provides a secure means for one window to send a string of data to another window, which need not be within the same domain as the first.
		@throws DOMError
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

	/**
		Returns the selection object representing the selected item(s).
		@throws DOMError
	**/
	function getSelection() : Selection;

	/**
		Gets computed style for the specified element. Computed style indicates the computed values of all CSS properties of the element.
		@throws DOMError
	**/
	function getComputedStyle( elt : Element, pseudoElt : String = "" ) : CSSStyleDeclaration;

	/**
		Returns a `MediaQueryList` object representing the specified media query string.
		@throws DOMError
	**/
	function matchMedia( query : String ) : MediaQueryList;

	/**
		Moves the window to the specified coordinates.
		@throws DOMError
	**/
	function moveTo( x : Int, y : Int ) : Void;

	/**
		Moves the current window by a specified amount.
		@throws DOMError
	**/
	function moveBy( x : Int, y : Int ) : Void;

	/**
		Dynamically resizes window.
		@throws DOMError
	**/
	function resizeTo( x : Int, y : Int ) : Void;

	/**
		Resizes the current window by a certain amount.
		@throws DOMError
	**/
	function resizeBy( x : Int, y : Int ) : Void;

	/**
		Scrolls the window to a particular place in the document.
	**/
	@:overload( function( x : Float, y : Float ) : Void {} )
	function scroll( ?options : ScrollToOptions ) : Void;

	/**
		Scrolls to a particular set of coordinates in the document.
	**/
	@:overload( function( x : Float, y : Float ) : Void {} )
	function scrollTo( ?options : ScrollToOptions ) : Void;

	/**
		Scrolls the document in the window by the given amount.
	**/
	@:overload( function( x : Float, y : Float ) : Void {} )
	function scrollBy( ?options : ScrollToOptions ) : Void;

	/**
		Tells the browser that an animation is in progress, requesting that the browser schedule a repaint of the window for the next animation frame.
		@throws DOMError
	**/
	function requestAnimationFrame( callback : Float -> Void ) : Int;

	/**
		Enables you to cancel a callback previously scheduled with `Window.requestAnimationFrame`.
		@throws DOMError
	**/
	function cancelAnimationFrame( handle : Int ) : Void;

	/**
		Gets default computed style for the specified element, ignoring author stylesheets.
		@throws DOMError
	**/
	function getDefaultComputedStyle( elt : Element, pseudoElt : String = "" ) : CSSStyleDeclaration;

	/**
		Scrolls the document by the given number of lines.
	**/
	function scrollByLines( numLines : Int, ?options : ScrollOptions ) : Void;

	/**
		Scrolls the current document by the specified number of pages.
	**/
	function scrollByPages( numPages : Int, ?options : ScrollOptions ) : Void;

	/**
		Sizes the window according to its content.
		@throws DOMError
	**/
	function sizeToContent() : Void;

	/**
		Updates the state of commands of the current chrome window (UI).
	**/
	function updateCommands( action : String, ?sel : Selection, reason : Int = 0 ) : Void;

	/**
		Searches for a given string in a window.
		@throws DOMError
	**/
	function find( str : String = "", caseSensitive : Bool = false, backwards : Bool = false, wrapAround : Bool = false, wholeWord : Bool = false, searchInFrames : Bool = false, showDialog : Bool = false ) : Bool;

	/**
		Writes a message to the console.
	**/
	function dump( str : String ) : Void;

	/**
		Creates a deep clone of a given value using the structured clone algorithm.
	**/
	function structuredClone<T>(value: T, ?options: {transfer: Array<Any>}): T;

	/**
		Toggles a user's ability to resize a window.
	**/
	function setResizable( resizable : Bool ) : Void;
	/** @throws DOMError */
	function btoa( btoa : String ) : String;
	/** @throws DOMError */
	function atob( atob : String ) : String;
	/** @throws DOMError */
	@:overload( function( handler : haxe.Constraints.Function, timeout : Float = 0, arguments : haxe.extern.Rest<Dynamic> ) : Int {} )
	function setTimeout( handler : String, timeout : Float = 0, unused : haxe.extern.Rest<Dynamic> ) : Int;
	function clearTimeout( handle : Int = 0 ) : Void;
	/** @throws DOMError */
	@:overload( function( handler : haxe.Constraints.Function, timeout : Float = 0, arguments : haxe.extern.Rest<Dynamic> ) : Int {} )
	function setInterval( handler : String, timeout : Float = 0, unused : haxe.extern.Rest<Dynamic> ) : Int;
	function clearInterval( handle : Int = 0 ) : Void;
	/** @throws DOMError */
	@:overload( function( aImage : VideoElement) : Promise<ImageBitmap> {} )
	@:overload( function( aImage : CanvasElement) : Promise<ImageBitmap> {} )
	@:overload( function( aImage : Blob) : Promise<ImageBitmap> {} )
	@:overload( function( aImage : ImageData) : Promise<ImageBitmap> {} )
	@:overload( function( aImage : CanvasRenderingContext2D) : Promise<ImageBitmap> {} )
	@:overload( function( aImage : ImageBitmap) : Promise<ImageBitmap> {} )
	@:overload( function( aImage : js.lib.ArrayBufferView) : Promise<ImageBitmap> {} )
	@:overload( function( aImage : js.lib.ArrayBuffer) : Promise<ImageBitmap> {} )
	@:overload( function( aImage : VideoElement, aSx : Int, aSy : Int, aSw : Int, aSh : Int) : Promise<ImageBitmap> {} )
	@:overload( function( aImage : CanvasElement, aSx : Int, aSy : Int, aSw : Int, aSh : Int) : Promise<ImageBitmap> {} )
	@:overload( function( aImage : Blob, aSx : Int, aSy : Int, aSw : Int, aSh : Int) : Promise<ImageBitmap> {} )
	@:overload( function( aImage : ImageData, aSx : Int, aSy : Int, aSw : Int, aSh : Int) : Promise<ImageBitmap> {} )
	@:overload( function( aImage : CanvasRenderingContext2D, aSx : Int, aSy : Int, aSw : Int, aSh : Int) : Promise<ImageBitmap> {} )
	@:overload( function( aImage : ImageBitmap, aSx : Int, aSy : Int, aSw : Int, aSh : Int) : Promise<ImageBitmap> {} )
	@:overload( function( aImage : js.lib.ArrayBufferView, aSx : Int, aSy : Int, aSw : Int, aSh : Int) : Promise<ImageBitmap> {} )
	@:overload( function( aImage : js.lib.ArrayBuffer, aSx : Int, aSy : Int, aSw : Int, aSh : Int) : Promise<ImageBitmap> {} )
	@:overload( function( aImage : VideoElement, aOffset : Int, aLength : Int, aFormat : ImageBitmapFormat, aLayout : Array<ChannelPixelLayout>) : Promise<ImageBitmap> {} )
	@:overload( function( aImage : CanvasElement, aOffset : Int, aLength : Int, aFormat : ImageBitmapFormat, aLayout : Array<ChannelPixelLayout>) : Promise<ImageBitmap> {} )
	@:overload( function( aImage : Blob, aOffset : Int, aLength : Int, aFormat : ImageBitmapFormat, aLayout : Array<ChannelPixelLayout>) : Promise<ImageBitmap> {} )
	@:overload( function( aImage : ImageData, aOffset : Int, aLength : Int, aFormat : ImageBitmapFormat, aLayout : Array<ChannelPixelLayout>) : Promise<ImageBitmap> {} )
	@:overload( function( aImage : CanvasRenderingContext2D, aOffset : Int, aLength : Int, aFormat : ImageBitmapFormat, aLayout : Array<ChannelPixelLayout>) : Promise<ImageBitmap> {} )
	@:overload( function( aImage : ImageBitmap, aOffset : Int, aLength : Int, aFormat : ImageBitmapFormat, aLayout : Array<ChannelPixelLayout>) : Promise<ImageBitmap> {} )
	@:overload( function( aImage : js.lib.ArrayBufferView, aOffset : Int, aLength : Int, aFormat : ImageBitmapFormat, aLayout : Array<ChannelPixelLayout>) : Promise<ImageBitmap> {} )
	@:overload( function( aImage : js.lib.ArrayBuffer, aOffset : Int, aLength : Int, aFormat : ImageBitmapFormat, aLayout : Array<ChannelPixelLayout>) : Promise<ImageBitmap> {} )
	@:overload( function( aImage : ImageElement ) : Promise<ImageBitmap> {} )
	@:overload( function( aImage : ImageElement, aSx : Int, aSy : Int, aSw : Int, aSh : Int ) : Promise<ImageBitmap> {} )
	function createImageBitmap( aImage : ImageElement, aOffset : Int, aLength : Int, aFormat : ImageBitmapFormat, aLayout : Array<ChannelPixelLayout> ) : Promise<ImageBitmap>;
	@:overload( function( input : String, ?init : RequestInit) : Promise<Response> {} )
	function fetch( input : Request, ?init : RequestInit ) : Promise<Response>;
}