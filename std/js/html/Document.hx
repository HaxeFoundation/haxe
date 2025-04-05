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

// This file is generated from mozilla\Document.webidl. Do not edit!

package js.html;

/**
	The `Document` interface represents any web page loaded in the browser and serves as an entry point into the web page's content, which is the DOM tree.`HTMLElement`

	Documentation [Document](https://developer.mozilla.org/en-US/docs/Web/API/Document) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/Document$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/Document>
**/
@:native("Document")
extern class Document extends Node {
	
	/**
		Returns the DOM implementation associated with the current document.
	**/
	var implementation(default,null) : DOMImplementation;
	
	/**
		Returns the document location as a string.
	**/
	var URL(default,null) : String;
	
	/**
		Returns the document location as a string.
	**/
	var documentURI(default,null) : String;
	
	/**
		Indicates whether the document is rendered in quirks or strict mode.
	**/
	var compatMode(default,null) : String;
	
	/**
		Returns the character set being used by the document.
	**/
	var characterSet(default,null) : String;
	
	/**
		Alias of `Document.characterSet`. Use this property instead.
	**/
	var charset(default,null) : String;
	
	/**
		Alias of `Document.characterSet`. Use this property instead.
	**/
	var inputEncoding(default,null) : String;
	
	/**
		Returns the Content-Type from the MIME Header of the current document.
	**/
	var contentType(default,null) : String;
	
	/**
		Returns the Document Type Definition (DTD) of the current document.
	**/
	var doctype(default,null) : DocumentType;
	
	/**
		Returns the `Element` that is a direct child of the document. For HTML documents, this is normally the `HTMLElement` element.
	**/
	var documentElement(default,null) : Element;
	
	/**
		Returns the URI of the current document.
	**/
	var location(default,null) : Location;
	
	/**
		Returns the URI of the page that linked to this page.
	**/
	var referrer(default,null) : String;
	
	/**
		Returns the date on which the document was last modified.
	**/
	var lastModified(default,null) : String;
	
	/**
		Returns loading status of the document.
	**/
	var readyState(default,null) : String;
	
	/**
		Sets or gets the title of the current document.
	**/
	var title : String;
	
	/**
		Gets/sets directionality (rtl/ltr) of the document.
	**/
	var dir : String;
	
	/**
		Returns the `body` or `frameset` node of the current document.
	**/
	var body : Element;
	
	/**
		Returns the `head` element of the current document.
	**/
	var head(default,null) : HeadElement;
	
	/**
		Returns a list of the images in the current document.
	**/
	var images(default,null) : HTMLCollection;
	
	/**
		Returns a list of the embedded `embed` elements within the current document.
	**/
	var embeds(default,null) : HTMLCollection;
	
	/**
		Returns a list of the available plugins.
	**/
	var plugins(default,null) : HTMLCollection;
	
	/**
		Returns a list of all the hyperlinks in the document.
	**/
	var links(default,null) : HTMLCollection;
	
	/**
		Returns a list of the `form` elements within the current document.
	**/
	var forms(default,null) : HTMLCollection;
	
	/**
		Returns all the `script` elements on the document.
	**/
	var scripts(default,null) : HTMLCollection;
	
	/**
		Returns a reference to the window object.
	**/
	var defaultView(default,null) : Window;
	
	/**
		Represents the event handling code for the `readystatechange` event.
	**/
	var onreadystatechange : haxe.Constraints.Function;
	
	/**
		Represents the event handling code for the `beforescriptexecute` event.
	**/
	var onbeforescriptexecute : haxe.Constraints.Function;
	
	/**
		Represents the event handling code for the `afterscriptexecute` event.
	**/
	var onafterscriptexecute : haxe.Constraints.Function;
	
	/**
		Is an `EventHandler` representing the code to be called when the `selectionchange` event is raised.
	**/
	var onselectionchange : haxe.Constraints.Function;
	var currentScript(default,null) : Element;
	
	/**
		Returns a list of all of the anchors in the document.
	**/
	var anchors(default,null) : HTMLCollection;
	
	/**
		Returns an ordered list of the applets within a document.
	**/
	var applets(default,null) : HTMLCollection;
	
	/**
		`true` when the document is in `Using_full-screen_mode`.
	**/
	var fullscreen(default,null) : Bool;
	var fullscreenEnabled(default,null) : Bool;
	
	/**
		Is an `EventHandler` representing the code to be called when the `fullscreenchange` event is raised.
	**/
	var onfullscreenchange : haxe.Constraints.Function;
	
	/**
		Is an `EventHandler` representing the code to be called when the `fullscreenerror` event is raised.
	**/
	var onfullscreenerror : haxe.Constraints.Function;
	
	/**
		Represents the event handling code for the `pointerlockchange` event.
	**/
	var onpointerlockchange : haxe.Constraints.Function;
	
	/**
		Represents the event handling code for the `pointerlockerror` event.
	**/
	var onpointerlockerror : haxe.Constraints.Function;
	
	/**
		…
	**/
	var hidden(default,null) : Bool;
	
	/**
		Returns a `string` denoting the visibility state of the document. Possible values are `visible`,  `hidden`,  `prerender`, and `unloaded`.
	**/
	var visibilityState(default,null) : VisibilityState;
	
	/**
		Is an `EventHandler` representing the code to be called when the `visibilitychange` event is raised.
	**/
	var onvisibilitychange : haxe.Constraints.Function;
	
	/**
		Returns which style sheet set is currently in use.
	**/
	var selectedStyleSheetSet : String;
	
	/**
		Returns the name of the style sheet set that was last enabled. Has the value `null` until the style sheet is changed by setting the value of `document.selectedStyleSheetSet`.
	**/
	var lastStyleSheetSet(default,null) : String;
	
	/**
		Returns the preferred style sheet set as specified by the page author.
	**/
	var preferredStyleSheetSet(default,null) : String;
	
	/**
		Returns a list of the style sheet sets available on the document.
	**/
	var styleSheetSets(default,null) : DOMStringList;
	
	/**
		Returns a reference to the `Element` that scrolls the document.
	**/
	var scrollingElement(default,null) : Element;
	
	/**
		…
	**/
	var timeline(default,null) : DocumentTimeline;
	var rootElement(default,null) : js.html.svg.SVGElement;
	
	/**
		Represents the event handling code for the `copy` event.
	**/
	var oncopy : haxe.Constraints.Function;
	
	/**
		Represents the event handling code for the `cut` event.
	**/
	var oncut : haxe.Constraints.Function;
	
	/**
		Represents the event handling code for the `paste` event.
	**/
	var onpaste : haxe.Constraints.Function;
	var activeElement(default,null) : Element;
	var styleSheets(default,null) : StyleSheetList;
	var pointerLockElement(default,null) : Element;
	
	/**
		The element that's currently in full screen mode for this document.
	**/
	var fullscreenElement(default,null) : Element;
	var fonts(default,null) : FontFaceSet;
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
	
	/**
		Represents the event handling code for the `wheel` event.
	**/
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
	var onerror : haxe.Constraints.Function;
	var children(default,null) : HTMLCollection;
	var firstElementChild(default,null) : Element;
	var lastElementChild(default,null) : Element;
	var childElementCount(default,null) : Int;
	var ontouchstart : haxe.Constraints.Function;
	var ontouchend : haxe.Constraints.Function;
	var ontouchmove : haxe.Constraints.Function;
	var ontouchcancel : haxe.Constraints.Function;
	
	/** @throws DOMError */
	function new() : Void;
	
	/**
		Returns a list of elements with the given tag name.
	**/
	@:pure
	function getElementsByTagName( localName : String ) : HTMLCollection;
	
	/**
		Returns a list of elements with the given tag name and namespace.
		@throws DOMError
	**/
	@:pure
	function getElementsByTagNameNS( namespace : String, localName : String ) : HTMLCollection;
	
	/**
		Returns a list of elements with the given class name.
	**/
	@:pure
	function getElementsByClassName( classNames : String ) : HTMLCollection;
	@:pure
	function getElementById( elementId : String ) : Element;
	
	/**
		Creates a new element with the given tag name.
		@throws DOMError
	**/
	@:overload( function( localName : String, ?options : String) : Element {} )
	function createElement( localName : String, ?options : ElementCreationOptions ) : Element;
	
	/**
		Creates a new element with the given tag name and namespace URI.
		@throws DOMError
	**/
	@:overload( function( namespace : String, qualifiedName : String, ?options : String) : Element {} )
	function createElementNS( namespace : String, qualifiedName : String, ?options : ElementCreationOptions ) : Element;
	
	/**
		Creates a new document fragment.
	**/
	function createDocumentFragment() : DocumentFragment;
	
	/**
		Creates a text node.
	**/
	function createTextNode( data : String ) : Text;
	
	/**
		Creates a new comment node and returns it.
	**/
	function createComment( data : String ) : Comment;
	
	/**
		Creates a new `ProcessingInstruction` object.
		@throws DOMError
	**/
	function createProcessingInstruction( target : String, data : String ) : ProcessingInstruction;
	
	/**
		Returns a clone of a node from an external document.
		@throws DOMError
	**/
	function importNode( node : Node, deep : Bool = false ) : Node;
	
	/**
		Adopt node from an external document.
		@throws DOMError
	**/
	function adoptNode( node : Node ) : Node;
	
	/**
		Creates an event object.
		@throws DOMError
	**/
	function createEvent( interface_ : String ) : Event;
	
	/**
		Creates a `Range` object.
		@throws DOMError
	**/
	function createRange() : Range;
	
	/**
		Creates a `NodeIterator` object.
		@throws DOMError
	**/
	@:overload( function( root : Node, whatToShow : Int = cast 4294967295, ?filter : haxe.Constraints.Function) : NodeIterator {} )
	@:overload( function( root : Node, whatToShow : Int = cast 4294967295, ?filter : NodeFilter) : NodeIterator {} )
	function createNodeIterator( root : Node, whatToShow : Int = cast 4294967295, ?filter : Node -> Int ) : NodeIterator;
	
	/**
		Creates a `TreeWalker` object.
		@throws DOMError
	**/
	@:overload( function( root : Node, whatToShow : Int = cast 4294967295, ?filter : haxe.Constraints.Function) : TreeWalker {} )
	@:overload( function( root : Node, whatToShow : Int = cast 4294967295, ?filter : NodeFilter) : TreeWalker {} )
	function createTreeWalker( root : Node, whatToShow : Int = cast 4294967295, ?filter : Node -> Int ) : TreeWalker;
	
	/**
		Creates a new CDATA node and returns it.
		@throws DOMError
	**/
	function createCDATASection( data : String ) : CDATASection;
	
	/**
		Creates a new `Attr` object and returns it.
		@throws DOMError
	**/
	function createAttribute( name : String ) : Attr;
	
	/**
		Creates a new attribute node in a given namespace and returns it.
		@throws DOMError
	**/
	function createAttributeNS( namespace : String, name : String ) : Attr;
	@:pure
	function getElementsByName( elementName : String ) : NodeList;
	
	/**
		Returns `true` if the focus is currently located anywhere inside the specified document.
		@throws DOMError
	**/
	function hasFocus() : Bool;
	
	/**
		Releases the current mouse capture if it's on an element in this document.
	**/
	function releaseCapture() : Void;

	/**
		Requests that the element on this document which is currently being presented in fullscreen mode
		be taken out of fullscreen mode, restoring the previous state of the screen.
	**/
	function exitFullscreen() : js.lib.Promise<Void>;

	/**
		Release the pointer lock.
	**/
	function exitPointerLock() : Void;
	
	/**
		Enables the style sheets for the specified style sheet set.
	**/
	function enableStyleSheetsForSet( name : String ) : Void;
	function caretPositionFromPoint( x : Float, y : Float ) : CaretPosition;
	/** @throws DOMError */
	@:pure
	function querySelector( selectors : String ) : Element;
	/** @throws DOMError */
	@:pure
	function querySelectorAll( selectors : String ) : NodeList;
	
	/**
		Returns an array of all `Animation` objects currently in effect, whose target elements are descendants of the `document`.
	**/
	function getAnimations() : Array<Animation>;
	
	/**
		Creates a `Touch` object.
	**/
	function createTouch( ?view : Window, ?target : EventTarget, identifier : Int = 0, pageX : Int = 0, pageY : Int = 0, screenX : Int = 0, screenY : Int = 0, clientX : Int = 0, clientY : Int = 0, radiusX : Int = 0, radiusY : Int = 0, rotationAngle : Float = 0.0, force : Float = 0.0 ) : Touch;
	
	/**
		Creates a `TouchList` object.
	**/
	@:overload( function( touch : Touch, touches : haxe.extern.Rest<Touch> ) : TouchList {} )
	@:overload( function() : TouchList {} )
	function createTouchList( touches : Array<Touch> ) : TouchList;
	/** @throws DOMError */
	function getSelection() : Selection;
	function elementFromPoint( x : Float, y : Float ) : Element;
	function elementsFromPoint( x : Float, y : Float ) : Array<Element>;
	/** @throws DOMError */
	@:overload( function( quad : DOMQuad, from : Element, ?options : ConvertCoordinateOptions) : DOMQuad {} )
	@:overload( function( quad : DOMQuad, from : HTMLDocument, ?options : ConvertCoordinateOptions) : DOMQuad {} )
	function convertQuadFromNode( quad : DOMQuad, from : Text, ?options : ConvertCoordinateOptions ) : DOMQuad;
	/** @throws DOMError */
	@:overload( function( rect : DOMRectReadOnly, from : Element, ?options : ConvertCoordinateOptions) : DOMQuad {} )
	@:overload( function( rect : DOMRectReadOnly, from : HTMLDocument, ?options : ConvertCoordinateOptions) : DOMQuad {} )
	function convertRectFromNode( rect : DOMRectReadOnly, from : Text, ?options : ConvertCoordinateOptions ) : DOMQuad;
	/** @throws DOMError */
	@:overload( function( point : DOMPointInit, from : Element, ?options : ConvertCoordinateOptions) : DOMPoint {} )
	@:overload( function( point : DOMPointInit, from : HTMLDocument, ?options : ConvertCoordinateOptions) : DOMPoint {} )
	function convertPointFromNode( point : DOMPointInit, from : Text, ?options : ConvertCoordinateOptions ) : DOMPoint;
	/** @throws DOMError */
	@:overload( function( nodes : haxe.extern.Rest<String>) : Void {} )
	function prepend( nodes : haxe.extern.Rest<Node> ) : Void;
	/** @throws DOMError */
	@:overload( function( nodes : haxe.extern.Rest<String>) : Void {} )
	function append( nodes : haxe.extern.Rest<Node> ) : Void;
	/** @throws DOMError */
	@:overload( function( expression : String, ?resolver : haxe.Constraints.Function) : XPathExpression {} )
	@:overload( function( expression : String, ?resolver : XPathNSResolver) : XPathExpression {} )
	function createExpression( expression : String, ?resolver : String -> Null<String> ) : XPathExpression;
	@:pure
	function createNSResolver( nodeResolver : Node ) : Node;
	/** @throws DOMError */
	@:overload( function( expression : String, contextNode : Node, ?resolver : haxe.Constraints.Function, type : Int = 0, ?result : Dynamic) : XPathResult {} )
	@:overload( function( expression : String, contextNode : Node, ?resolver : XPathNSResolver, type : Int = 0, ?result : Dynamic) : XPathResult {} )
	function evaluate( expression : String, contextNode : Node, ?resolver : String -> Null<String>, type : Int = 0, ?result : Dynamic ) : XPathResult;
}