/*
 * Copyright (C)2005-2013 Haxe Foundation
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

// This file is generated, do not edit!
package js.html;

/** <p>This section provides a brief reference for all of the methods, properties, and events available through the DOM <code>window</code> object. The <code>window</code> object implements the <code>Window</code> interface, which in turn inherits from the <code><a class="external" rel="external" href="http://www.w3.org/TR/DOM-Level-2-Views/views.html#Views-AbstractView" title="http://www.w3.org/TR/DOM-Level-2-Views/views.html#Views-AbstractView" target="_blank">AbstractView</a></code> interface. Some additional global functions, namespaces objects, and constructors, not typically associated with the window, but available on it, are listed in the <a title="https://developer.mozilla.org/en/JavaScript/Reference" rel="internal" href="https://developer.mozilla.org/en/JavaScript/Reference">JavaScript Reference</a>.</p>
<p>The <code>window</code> object represents the window itself. The <code>document</code> property of a <code>window</code> points to the <a title="en/DOM/document" rel="internal" href="https://developer.mozilla.org/en/DOM/document">DOM document</a> loaded in that window. A window for a given document can be obtained using the <code><a rel="custom" href="https://developer.mozilla.org/en/DOM/document.defaultView">document.defaultView</a></code>
 property.</p>
<p>In a tabbed browser, such as Firefox, each tab contains its own <code>window</code> object (and if you're writing an extension, the browser window itself is a separate window too - see <a title="en/Working_with_windows_in_chrome_code#Content_windows" rel="internal" href="https://developer.mozilla.org/en/Working_with_windows_in_chrome_code#Content_windows">Working with windows in chrome code</a> for more information). That is, the <code>window</code> object is not shared between tabs in the same window. Some methods, namely <code><a rel="custom" href="https://developer.mozilla.org/en/DOM/window.resizeTo">window.resizeTo</a></code>
 and <code><a rel="custom" href="https://developer.mozilla.org/en/DOM/window.resizeBy">window.resizeBy</a></code>
 apply to the whole window and not to the specific tab the <code>window</code> object belongs to. Generally, anything that can't reasonably pertain to a tab pertains to the window instead.</p><br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/window">MDN</a>. */
@:native("Window")
extern class DOMWindow extends EventTarget
{
	static inline var PERSISTENT : Int = 1;

	static inline var TEMPORARY : Int = 0;

	/** An <code><a rel="custom" href="https://developer.mozilla.org/en/nsIDOMOfflineResourceList">nsIDOMOfflineResourceList</a></code>
 object providing access to the offline resources for the window. */
	var applicationCache(default,null) : DOMApplicationCache;

	var clientInformation(default,null) : Navigator;

	/** <dd>This property indicates whether the current window is closed or not.</dd> <dt><a title="en/Components_object" rel="internal" href="https://developer.mozilla.org/en/Components_object">window.Components</a></dt> <dd>The entry point to many <a title="en/XPCOM" rel="internal" href="https://developer.mozilla.org/en/XPCOM">XPCOM</a> features. Some properties, e.g. <a title="en/Components.classes" rel="internal" href="https://developer.mozilla.org/en/Components.classes">classes</a>, are only available to sufficiently privileged code.</dd> */
	var closed(default,null) : Bool;

	var console(default,null) : Console;

	/** Returns the browser crypto object. */
	var crypto(default,null) : Crypto;

	/** Gets/sets the status bar text for the given window. */
	var defaultStatus : String;

	var defaultstatus : String;

	var devicePixelRatio(default,null) : Float;

	/** Returns a reference to the document that the window contains. */
	var document(default,null) : Document;

	var event(default,null) : Event;

	/** Returns the element in which the window is embedded, or null if the window is not embedded. */
	var frameElement(default,null) : Element;

	/** Returns an array of the subframes in the current window. */
	var frames(default,null) : DOMWindow;

	/** Returns a reference to the history object. */
	var history(default,null) : History;

	var indexedDB(default,null) : js.html.idb.Factory;

	/** Gets the height of the content area of the browser window including, if rendered, the horizontal scrollbar. */
	var innerHeight(default,null) : Int;

	/** Gets the width of the content area of the browser window including, if rendered, the vertical scrollbar. */
	var innerWidth(default,null) : Int;

	/** Returns the number of frames in the window. See also <code><a rel="custom" href="https://developer.mozilla.org/en/DOM/window.frames">window.frames</a></code>
. */
	var length(default,null) : Int;

	/** Returns a reference to the local storage object used to store data that may only be accessed by the origin that created it. Getter throws DOMException. */
	var localStorage(default,null) : Storage;

	/** Gets/sets the location, or current URL, of the window object. */
	var location : Location;

	/** Returns the locationbar object, whose visibility can be toggled in the window. */
	var locationbar(default,null) : BarInfo;

	/** Returns the menubar object, whose visibility can be toggled in the window. */
	var menubar(default,null) : BarInfo;

	/** Gets/sets the name of the window. */
	var name : String;

	/** Returns a reference to the navigator object. */
	var navigator(default,null) : Navigator;

	var notifications(default,null) : NotificationCenter;

	var offscreenBuffering(default,null) : Bool;

	/** An event handler property for abort events on the window. */
	var onabort : EventListener;

	var onanimationend : EventListener;

	var onanimationiteration : EventListener;

	var onanimationstart : EventListener;

	/** An event handler property for before-unload events on the window. */
	var onbeforeunload : EventListener;

	/** An event handler property for blur events on the window. */
	var onblur : EventListener;

	var oncanplay : EventListener;

	var oncanplaythrough : EventListener;

	/** An event handler property for change events on the window. */
	var onchange : EventListener;

	/** An event handler property for click events on the window. */
	var onclick : EventListener;

	/** An event handler property for right-click events on the window. */
	var oncontextmenu : EventListener;

	var ondblclick : EventListener;

	var ondevicemotion : EventListener;

	var ondeviceorientation : EventListener;

	var ondrag : EventListener;

	var ondragend : EventListener;

	var ondragenter : EventListener;

	var ondragleave : EventListener;

	var ondragover : EventListener;

	var ondragstart : EventListener;

	var ondrop : EventListener;

	var ondurationchange : EventListener;

	var onemptied : EventListener;

	var onended : EventListener;

	/** An event handler property for errors raised on the window. */
	var onerror : EventListener;

	/** An event handler property for focus events on the window. */
	var onfocus : EventListener;

	/** An event handler property for hash change events on the window; called when the part of the URL after the hash mark ("#") changes. */
	var onhashchange : EventListener;

	var oninput : EventListener;

	var oninvalid : EventListener;

	/** An event handler property for keydown events on the window. */
	var onkeydown : EventListener;

	/** An event handler property for keypress events on the window. */
	var onkeypress : EventListener;

	/** An event handler property for keyup events on the window. */
	var onkeyup : EventListener;

	/** An event handler property for window loading. */
	var onload : EventListener;

	var onloadeddata : EventListener;

	var onloadedmetadata : EventListener;

	var onloadstart : EventListener;

	var onmessage : EventListener;

	/** An event handler property for mousedown events on the window. */
	var onmousedown : EventListener;

	/** An event handler property for mousemove events on the window. */
	var onmousemove : EventListener;

	/** An event handler property for mouseout events on the window. */
	var onmouseout : EventListener;

	/** An event handler property for mouseover events on the window. */
	var onmouseover : EventListener;

	/** An event handler property for mouseup events on the window. */
	var onmouseup : EventListener;

	var onmousewheel : EventListener;

	var onoffline : EventListener;

	var ononline : EventListener;

	/** An event handler property for pagehide events on the window. */
	var onpagehide : EventListener;

	/** An event handler property for pageshow events on the window. */
	var onpageshow : EventListener;

	var onpause : EventListener;

	var onplay : EventListener;

	var onplaying : EventListener;

	/** An event handler property for popstate events, which are fired when navigating to a session history entry representing a state object. */
	var onpopstate : EventListener;

	var onprogress : EventListener;

	var onratechange : EventListener;

	/** An event handler property for reset events on the window. */
	var onreset : EventListener;

	/** An event handler property for window resizing. */
	var onresize : EventListener;

	/** An event handler property for window scrolling. */
	var onscroll : EventListener;

	var onsearch : EventListener;

	var onseeked : EventListener;

	var onseeking : EventListener;

	/** An event handler property for window selection. */
	var onselect : EventListener;

	var onstalled : EventListener;

	var onstorage : EventListener;

	/** An event handler property for submits on window forms. */
	var onsubmit : EventListener;

	var onsuspend : EventListener;

	var ontimeupdate : EventListener;

	var ontouchcancel : EventListener;

	var ontouchend : EventListener;

	var ontouchmove : EventListener;

	var ontouchstart : EventListener;

	var ontransitionend : EventListener;

	/** An event handler property for unload events on the window. */
	var onunload : EventListener;

	var onvolumechange : EventListener;

	var onwaiting : EventListener;

	/** Returns a reference to the window that opened this current window. */
	var opener(default,null) : DOMWindow;

	/** Gets the height of the outside of the browser window. */
	var outerHeight(default,null) : Int;

	/** Gets the width of the outside of the browser window. */
	var outerWidth(default,null) : Int;

	var pagePopupController(default,null) : PagePopupController;

	/** An alias for <code><a rel="custom" href="https://developer.mozilla.org/en/DOM/window.scrollX">window.scrollX</a></code>
. */
	var pageXOffset(default,null) : Int;

	/** An alias for <code><a rel="custom" href="https://developer.mozilla.org/en/DOM/window.scrollY">window.scrollY</a></code> */
	var pageYOffset(default,null) : Int;

	/** Returns a reference to the parent of the current window or subframe. */
	var parent(default,null) : DOMWindow;

	var performance(default,null) : Performance;

	/** Returns the personalbar object, whose visibility can be toggled in the window. */
	var personalbar(default,null) : BarInfo;

	/** Returns a reference to the screen object associated with the window. */
	var screen(default,null) : Screen;

	var screenLeft(default,null) : Int;

	var screenTop(default,null) : Int;

	/** Returns the horizontal distance of the left border of the user's browser from the left side of the screen. */
	var screenX(default,null) : Int;

	/** Returns the vertical distance of the top border of the user's browser from the top side of the screen. */
	var screenY(default,null) : Int;

	/** Returns the number of pixels that the document has already been scrolled horizontally. */
	var scrollX(default,null) : Int;

	/** Returns the number of pixels that the document has already been scrolled vertically. */
	var scrollY(default,null) : Int;

	/** Returns the scrollbars object, whose visibility can be toggled in the window. */
	var scrollbars(default,null) : BarInfo;

	/** Returns an object reference to the window object itself. */
	var self(default,null) : DOMWindow;

	/** A storage object for storing data within a single page session. Getter throws DOMException. */
	var sessionStorage(default,null) : Storage;

	/** Gets/sets the text in the statusbar at the bottom of the browser. */
	var status : String;

	/** Returns the statusbar object, whose visibility can be toggled in the window. */
	var statusbar(default,null) : BarInfo;

	var storageInfo(default,null) : StorageInfo;

	var styleMedia(default,null) : StyleMedia;

	/** Returns the toolbar object, whose visibility can be toggled in the window. */
	var toolbar(default,null) : BarInfo;

	/** <dd>Returns a reference to the topmost window in the window hierarchy. This property is read only.</dd> <dt><code><a rel="custom" href="https://developer.mozilla.org/en/DOM/window.URL">window.URL</a></code>
 
<span title="(Firefox 4 / Thunderbird 3.3 / SeaMonkey 2.1)
">Requires Gecko 2.0</span>
</dt> <dd>A DOM&nbsp;URL&nbsp;object, which provides the <code><a rel="custom" href="https://developer.mozilla.org/en/DOM/window.URL.createObjectURL">window.URL.createObjectURL()</a></code>
 and <code><a rel="custom" href="https://developer.mozilla.org/en/DOM/window.URL.revokeObjectURL">window.URL.revokeObjectURL()</a></code>
 methods.</dd> */
	var top(default,null) : DOMWindow;

	/** <dd>Returns a reference to the current window.</dd> <dt>window[0], window[1], etc.</dt> <dd>Returns a reference to the <code>window</code> object in the frames. See <code><a rel="custom" href="https://developer.mozilla.org/en/DOM/window.frames">window.frames</a></code>
 for more details.</dd> */
	var window(default,null) : DOMWindow;

	function alert( message : String ) : Void;

	function atob( string : String ) : String;

	function blur() : Void;

	function btoa( string : String ) : String;

	@:overload( function( id : Int ) :Void {} )
	function cancelAnimationFrame( id : Int ) : Void;

	function cancelRequestAnimationFrame( id : Int ) : Void;

	function captureEvents() : Void;

	function clearInterval( handle : Int ) : Void;

	function clearTimeout( handle : Int ) : Void;

	function close() : Void;

	function confirm( message : String ) : Bool;

	function convertPointFromNodeToPage( node : Node, p : Point ) : Point;

	function convertPointFromPageToNode( node : Node, p : Point ) : Point;

	function find( string : String, caseSensitive : Bool, backwards : Bool, wrap : Bool, wholeWord : Bool, searchInFrames : Bool, showDialog : Bool ) : Bool;

	function focus() : Void;

	function getComputedStyle( element : Element, pseudoElement : String ) : CSSStyleDeclaration;

	function getMatchedCSSRules( element : Element, pseudoElement : String ) : CSSRuleList;

	function getSelection() : DOMSelection;

	function matchMedia( query : String ) : MediaQueryList;

	function moveBy( x : Float, y : Float ) : Void;

	function moveTo( x : Float, y : Float ) : Void;

	function open( url : String, name : String, ?options : String ) : DOMWindow;

	function openDatabase( name : String, version : String, displayName : String, estimatedSize : Int, ?creationCallback : js.html.sql.DatabaseCallback ) : js.html.sql.Database;

	/** Provides a secure means for one window to send a string of data to another window, which need not be within the same domain as the first, in a secure manner. Throws DOMException. */
	@:overload( function( message : Dynamic, targetOrigin : String ) :Void {} )
	function postMessage( message : Dynamic, targetOrigin : String, messagePorts : Array<Dynamic> ) : Void;

	function print() : Void;

	function prompt( message : String, defaultValue : String ) : String;

	function releaseEvents() : Void;

	@:overload( function( callback_ : RequestAnimationFrameCallback ) :Int {} )
	function requestAnimationFrame( callback_ : RequestAnimationFrameCallback ) : Int;

	function requestFileSystem( type : Int, size : Int, successCallback : js.html.fs.FileSystemCallback, ?errorCallback : js.html.fs.ErrorCallback ) : Void;

	function resizeBy( x : Float, y : Float ) : Void;

	function resizeTo( width : Float, height : Float ) : Void;

	function resolveLocalFileSystemURL( url : String, successCallback : js.html.fs.EntryCallback, ?errorCallback : js.html.fs.ErrorCallback ) : Void;

	function scroll( x : Int, y : Int ) : Void;

	function scrollBy( x : Int, y : Int ) : Void;

	function scrollTo( x : Int, y : Int ) : Void;

	function setInterval( handler : Void->Void, timeout : Int ) : Int;

	function setTimeout( handler : Void->Void, timeout : Int ) : Int;

	function showModalDialog( url : String, ?dialogArgs : Dynamic, ?featureArgs : String ) : Dynamic;

	function stop() : Void;

}
