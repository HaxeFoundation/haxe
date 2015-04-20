/*
 * Copyright (C)2005-2015 Haxe Foundation
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

// This file is generated from mozilla/Window.webidl line 28:11. Do not edit!

package js.html;

@:native("Window")
extern class Window extends EventTarget
{
	var window(default,null) : Window;
	var self(default,null) : Window;
	var document(default,null) : HTMLDocument;
	var name : String;
	var location(default,null) : Location;
	var history(default,null) : History;
	var locationbar(default,null) : BarProp;
	var menubar(default,null) : BarProp;
	var personalbar(default,null) : BarProp;
	var scrollbars(default,null) : BarProp;
	var statusbar(default,null) : BarProp;
	var toolbar(default,null) : BarProp;
	var status : String;
	var closed(default,null) : Bool;
	var frames(default,null) : Window;
	var length(default,null) : Int;
	var top(default,null) : Window;
	var opener : Dynamic;
	var parent(default,null) : Window;
	var frameElement(default,null) : Element;
	var navigator(default,null) : Navigator;
	var applicationCache(default,null) : ApplicationCache;
	var orientation(default,null) : Int;
	var onorientationchange : haxe.Constraints.Function;
	var screen(default,null) : Screen;
	var innerWidth : Int;
	var innerHeight : Int;
	var scrollX(default,null) : Int;
	var pageXOffset(default,null) : Int;
	var scrollY(default,null) : Int;
	var pageYOffset(default,null) : Int;
	var screenX : Int;
	var screenY : Int;
	var outerWidth : Int;
	var outerHeight : Int;
	var performance(default,null) : Performance;
	var crypto(default,null) : Dynamic/*MISSING nsIDOMCrypto*/;
	var devicePixelRatio(default,null) : Float;
	var scrollMaxX(default,null) : Int;
	var scrollMaxY(default,null) : Int;
	var fullScreen : Bool;
	var onwheel : haxe.Constraints.Function;
	var ondevicemotion : haxe.Constraints.Function;
	var ondeviceorientation : haxe.Constraints.Function;
	var ondeviceproximity : haxe.Constraints.Function;
	var onuserproximity : haxe.Constraints.Function;
	var ondevicelight : haxe.Constraints.Function;
	var content(default,null) : Dynamic;
	var console(default,null) : Console;
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
	var onpointerlockchange : haxe.Constraints.Function;
	var onpointerlockerror : haxe.Constraints.Function;
	var indexedDB(default,null) : js.html.idb.Factory;
	var onerror : haxe.extern.EitherType<Event,String> -> String -> Int -> Int -> Dynamic -> Bool;
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
	var onresize : haxe.Constraints.Function;
	var onunload : haxe.Constraints.Function;
	var localStorage(default,null) : Storage;
	var sessionStorage(default,null) : Storage;
	
	/** @throws DOMError */
	function close() : Void;
	/** @throws DOMError */
	function stop() : Void;
	/** @throws DOMError */
	function focus() : Void;
	/** @throws DOMError */
	function blur() : Void;
	/** @throws DOMError */
	function open( ?url : String = "", ?target : String = "", ?features : String = "" ) : Window;
	/** @throws DOMError */
	@:overload( function() : Void {} )
	function alert( message : String ) : Void;
	/** @throws DOMError */
	function confirm( ?message : String = "" ) : Bool;
	/** @throws DOMError */
	function prompt( ?message : String = "", ?default_ : String = "" ) : String;
	/** @throws DOMError */
	function print() : Void;
	/** @throws DOMError */
	function postMessage( message : Dynamic, targetOrigin : String, ?transfer : Array<Dynamic> ) : Void;
	function captureEvents() : Void;
	function releaseEvents() : Void;
	/** @throws DOMError */
	function getSelection() : Selection;
	/** @throws DOMError */
	function getComputedStyle( elt : Element, ?pseudoElt : String = "" ) : CSSStyleDeclaration;
	/** @throws DOMError */
	function matchMedia( query : String ) : MediaQueryList;
	/** @throws DOMError */
	function moveTo( x : Int, y : Int ) : Void;
	/** @throws DOMError */
	function moveBy( x : Int, y : Int ) : Void;
	/** @throws DOMError */
	function resizeTo( x : Int, y : Int ) : Void;
	/** @throws DOMError */
	function resizeBy( x : Int, y : Int ) : Void;
	@:overload( function( x : Float, y : Float ) : Void {} )
	function scroll( ?options : ScrollToOptions ) : Void;
	@:overload( function( x : Float, y : Float ) : Void {} )
	function scrollTo( ?options : ScrollToOptions ) : Void;
	@:overload( function( x : Float, y : Float ) : Void {} )
	function scrollBy( ?options : ScrollToOptions ) : Void;
	/** @throws DOMError */
	function requestAnimationFrame( callback : Float -> Void ) : Int;
	/** @throws DOMError */
	function cancelAnimationFrame( handle : Int ) : Void;
	/** @throws DOMError */
	function getDefaultComputedStyle( elt : Element, ?pseudoElt : String = "" ) : CSSStyleDeclaration;
	function scrollByLines( numLines : Int, ?options : ScrollOptions ) : Void;
	function scrollByPages( numPages : Int, ?options : ScrollOptions ) : Void;
	/** @throws DOMError */
	function sizeToContent() : Void;
	function updateCommands( action : String, ?sel : Selection, ?reason : Int = 0 ) : Void;
	/** @throws DOMError */
	function find( ?str : String = "", ?caseSensitive : Bool = false, ?backwards : Bool = false, ?wrapAround : Bool = false, ?wholeWord : Bool = false, ?searchInFrames : Bool = false, ?showDialog : Bool = false ) : Bool;
	function dump( str : String ) : Void;
	function setResizable( resizable : Bool ) : Void;
	/** @throws DOMError */
	function btoa( btoa : String ) : String;
	/** @throws DOMError */
	function atob( atob : String ) : String;
	/** @throws DOMError */
	@:overload( function( handler : haxe.Constraints.Function, ?timeout : Int = 0, arguments : haxe.extern.Rest<Dynamic> ) : Int {} )
	function setTimeout( handler : String, ?timeout : Int = 0, unused : haxe.extern.Rest<Dynamic> ) : Int;
	/** @throws DOMError */
	function clearTimeout( ?handle : Int = 0 ) : Void;
	/** @throws DOMError */
	@:overload( function( handler : haxe.Constraints.Function, ?timeout : Int, arguments : haxe.extern.Rest<Dynamic> ) : Int {} )
	function setInterval( handler : String, ?timeout : Int, unused : haxe.extern.Rest<Dynamic> ) : Int;
	/** @throws DOMError */
	function clearInterval( ?handle : Int = 0 ) : Void;
}