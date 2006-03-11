/*
 * Copyright (c) 2005, The haXe Project Contributors
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE HAXE PROJECT CONTRIBUTORS "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE HAXE PROJECT CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 */
package js;

extern class Window {

	var history : History;
	var location : Location;
	var document : Document;
	var navigator : Navigator;
	var screen : Screen;

	var frames : HtmlCollection<Frame>;
	var closed : Bool;
	var defaultStatus : String;
	var length : Int;
	var name : String;
	var opener : Window;
	var parent : Window;

	var self : Window;
	var status : String;
	var top : Window;

	function alert( msg : String ) : Void;
	function blur() : Void;
	// clearInterval
	// clearTimeout
	function close() : Void;
	function confirm( msg : String ) : Bool;
	function focus() : Void;
	function moveBy( dx : Int, dy : Int ) : Void;
	function moveTo( x : Int, y : Int ) : Void;
	function print() : Void;
	function prompt( msg : String ) : String;
	// FF1.5 resizeTo
	function scrollBy( dx : Int, dy : Int ) : Void;
	function scrollTo( x : Int, y : Int ) : Void;
	// setInterval
	// setTimeout

	/* IE only ?
	clientInformation
	clipboardData
	event
	external
	dialogArguments
	dialog....
	frameElement
	offscreenBuffering
	returnValue
	screenTop
	createPopup()
	execScript()
	navigate(url)
	resizeBy()
	scroll();
	setActive()
	showHelp()
	show...()
	*/

 	// events : only on FF
}

