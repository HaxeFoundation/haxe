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

extern class Document extends HtmlDom {

	var anchors : HtmlCollection<Anchor>;
	// applets : Applet is deprecated in Dom2
	var forms : HtmlCollection<Form>;
	var images : HtmlCollection<Image>;
	var links : HtmlCollection<Link>;
	// plugins : Not in IE, not in W3C

	/* deprecated in Dom2 , use body
	var alinkColor;
	var background;
	var bgColor;
	var fgColor;
	var linkColor;
	var vlinkColor;
	*/

	var body : Body;
	var cookie : String;
	var domain : String;
	var referrer : String;
	var title : String;

	// TODO : var URL : String;

	#if w3c
	#else true
	// not W3C , need infos : var embeds : HtmlCollection<Embed>;
	var lastModified : Date;
	var stylesheets : HtmlCollection<StyleSheet>;
	function focus() : Void;
	function getElementsByTag( tag : String ) : HtmlCollection<HtmlDom>;
	#end

	function open() : Void;
	function write( str : String ) : Void;
	function writeln( str : String ) : Void;
	function close() : Void;
	function getElementById( id : String ) : HtmlDom;
	function getElementsByName( name : String ) : HtmlCollection<HtmlDom>;
	function createElement( name : String ) : HtmlDom;

	var onclick : Event -> Void;
	var ondblclick : Event -> Void;
	var onfocus : Event -> Void;
	var onkeydown : Event -> Void;
	var onkeyup : Event -> Void;
	var onmousedown : Event -> Void;
	var onmousemove : Event -> Void;
	var onmouseout : Event -> Void;
	var onmouseover : Event -> Void;
	var onmouseup : Event -> Void;
	var onresize : Event -> Void;

}
