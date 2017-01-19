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

// This file is generated from mozilla\Document.webidl. Do not edit!

package js.html;

/**
	The `Document` interface represents any web page loaded in the browser and serves as an entry point into the web page's content, which is the DOM tree. The DOM tree includes elements such as `body` and `table`, among many others. It provides functionality global to the document, like how to obtain the page's URL and create new elements in the document.

	Documentation [Document](https://developer.mozilla.org/en-US/docs/Web/API/Document) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/Document$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/Document>
**/
@:native("Document")
extern class Document extends Node
{
	
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
		Returns the `Element` that is a direct child of the document. For HTML documents, this is normally the `html` element.
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
		Sets or gets title of the current document.
	**/
	var title : String;
	
	/**
		Gets/sets directionality (rtl/ltr) of the document.
	**/
	var dir : String;
	
	/**
		Returns a reference to the window object.
	**/
	var defaultView(default,null) : Window;
	
	/**
		Returns the currently focused element.
	**/
	var activeElement(default,null) : Element;
	
	/**
		Represents the event handling code for the `readystatechange` event.
	**/
	var onreadystatechange : haxe.Constraints.Function;
	
	/**
		Represents the event handling code for the `wheel` event.
	**/
	var onwheel : haxe.Constraints.Function;
	
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
	
	/**
		Represents the event handling code for the `beforescriptexecute` event.
	**/
	var onbeforescriptexecute : haxe.Constraints.Function;
	
	/**
		Represents the event handling code for the `afterscriptexecute` event.
	**/
	var onafterscriptexecute : haxe.Constraints.Function;
	var currentScript(default,null) : Element;
	var fullscreenEnabled(default,null) : Bool;
	var fullscreenElement(default,null) : Element;
	
	/**
		Returns the element set as the target for mouse events while the pointer is locked. `null` if lock is pending, pointer is unlocked, or if the target is in another document.
	**/
	var pointerLockElement(default,null) : Element;
	
	/**
		…
	**/
	var hidden(default,null) : Bool;
	
	/**
		
		 Returns a `string` denoting the visibility state of the document. Possible values are `visible`,  `hidden`,  `prerender`, and `unloaded`.
		 
	**/
	var visibilityState(default,null) : VisibilityState;
	
	/**
		Returns a list of the style sheet objects on the current document.
	**/
	var styleSheets(default,null) : StyleSheetList;
	
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
		…
	**/
	var timeline(default,null) : DocumentTimeline;
	var fonts(default,null) : FontFaceSet;
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
		Represetnts the event handling code for the `pointerlockerror` event.
	**/
	var onpointerlockerror : haxe.Constraints.Function;
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
	function getElementsByTagName( localName : String ) : HTMLCollection;
	/** @throws DOMError */
	
	/**
		Returns a list of elements with the given tag name and namespace.
	**/
	function getElementsByTagNameNS( namespace_ : String, localName : String ) : HTMLCollection;
	
	/**
		Returns a list of elements with the given class name.
	**/
	function getElementsByClassName( classNames : String ) : HTMLCollection;
	function getElementById( elementId : String ) : Element;
	/** @throws DOMError */
	@:overload( function( localName : String ) : Element {} )
	
	/**
		Creates a new element with the given tag name.
	**/
	function createElement( localName : String, typeExtension : String ) : Element;
	/** @throws DOMError */
	@:overload( function( namespace_ : String, qualifiedName : String ) : Element {} )
	
	/**
		Creates a new element with the given tag name and namespace URI.
	**/
	function createElementNS( namespace_ : String, qualifiedName : String, typeExtension : String ) : Element;
	
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
	/** @throws DOMError */
	
	/**
		Creates a new `ProcessingInstruction` object.
	**/
	function createProcessingInstruction( target : String, data : String ) : ProcessingInstruction;
	/** @throws DOMError */
	
	/**
		Returns a clone of a node from an external document.
	**/
	function importNode( node : Node, ?deep : Bool = false ) : Node;
	/** @throws DOMError */
	
	/**
		Adopt node from an external document.
	**/
	function adoptNode( node : Node ) : Node;
	/** @throws DOMError */
	
	/**
		Creates an event object.
	**/
	function createEvent( interface_ : String ) : Event;
	/** @throws DOMError */
	
	/**
		Creates a `Range` object.
	**/
	function createRange() : Range;
	/** @throws DOMError */
	
	/**
		Creates a `NodeIterator` object.
	**/
	function createNodeIterator( root : Node, ?whatToShow : Int = cast 4294967295, ?filter : NodeFilter ) : NodeIterator;
	/** @throws DOMError */
	
	/**
		Creates a `TreeWalker` object.
	**/
	function createTreeWalker( root : Node, ?whatToShow : Int = cast 4294967295, ?filter : NodeFilter ) : TreeWalker;
	/** @throws DOMError */
	
	/**
		Creates a new CDATA node and returns it.
	**/
	function createCDATASection( data : String ) : CDATASection;
	/** @throws DOMError */
	
	/**
		Creates a new `Attr` object and returns it.
	**/
	function createAttribute( name : String ) : Attr;
	/** @throws DOMError */
	
	/**
		Creates a new attribute node in a given namespace and returns it.
	**/
	function createAttributeNS( namespace_ : String, name : String ) : Attr;
	/** @throws DOMError */
	function hasFocus() : Bool;
	
	/**
		Releases the current mouse capture if it's on an element in this document.
	**/
	function releaseCapture() : Void;
	function exitFullscreen() : Void;
	
	/**
		Release the pointer lock.
	**/
	function exitPointerLock() : Void;
	/** @throws DOMError */
	
	/**
		Registers a web component.
	**/
	function registerElement( name : String, ?options : ElementRegistrationOptions ) : Dynamic;
	
	/**
		Enables the style sheets for the specified style sheet set.
	**/
	function enableStyleSheetsForSet( name : String ) : Void;
	
	/**
		Returns the topmost element at the specified coordinates. 
	**/
	function elementFromPoint( x : Float, y : Float ) : Element;
	
	/**
		Returns an array of all elements at the specified coordinates.
	**/
	function elementsFromPoint( x : Float, y : Float ) : Array<Element>;
	
	/**
		Gets the `CaretPosition` at or near the specified coordinates.
	**/
	function caretPositionFromPoint( x : Float, y : Float ) : CaretPosition;
	/** @throws DOMError */
	function querySelector( selectors : String ) : Element;
	/** @throws DOMError */
	function querySelectorAll( selectors : String ) : NodeList;
	
	/**
		Returns an array of all `Animation` objects currently in effect whose target elements are descendants of the `document`.
	**/
	function getAnimations() : Array<Animation>;
	
	/**
		Creates a `Touch` object.
	**/
	function createTouch( ?view : Window, ?target : EventTarget, ?identifier : Int = 0, ?pageX : Int = 0, ?pageY : Int = 0, ?screenX : Int = 0, ?screenY : Int = 0, ?clientX : Int = 0, ?clientY : Int = 0, ?radiusX : Int = 0, ?radiusY : Int = 0, ?rotationAngle : Float = 0.0, ?force : Float = 0.0 ) : Touch;
	@:overload( function( touch : Touch, touches : haxe.extern.Rest<Touch> ) : TouchList {} )
	@:overload( function() : TouchList {} )
	
	/**
		Creates a `TouchList` object.
	**/
	function createTouchList( touches : Array<Touch> ) : TouchList;
	/** @throws DOMError */
	function convertQuadFromNode( quad : DOMQuad, from : haxe.extern.EitherType<Text,haxe.extern.EitherType<Element,HTMLDocument>>, ?options : ConvertCoordinateOptions ) : DOMQuad;
	/** @throws DOMError */
	function convertRectFromNode( rect : DOMRectReadOnly, from : haxe.extern.EitherType<Text,haxe.extern.EitherType<Element,HTMLDocument>>, ?options : ConvertCoordinateOptions ) : DOMQuad;
	/** @throws DOMError */
	function convertPointFromNode( point : DOMPointInit, from : haxe.extern.EitherType<Text,haxe.extern.EitherType<Element,HTMLDocument>>, ?options : ConvertCoordinateOptions ) : DOMPoint;
	/** @throws DOMError */
	function createExpression( expression : String, resolver : XPathNSResolver ) : XPathExpression;
	function createNSResolver( nodeResolver : Node ) : Node;
	/** @throws DOMError */
	function evaluate( expression : String, contextNode : Node, resolver : XPathNSResolver, type : Int, result : Dynamic ) : XPathResult;
}