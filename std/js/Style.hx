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

extern class Style {

	// Position
	var left : Int;
	var bottom : Int;
	var height : Int;
	var width : Int;
	var top : Int;
	var right : Int;
	var zIndex : Int;
	var position : Dynamic;

	// BG
	var background : Dynamic;
	var backgroundAttachment : Dynamic;
	var backgroundColor : Dynamic;
	var backgroundImage : Dynamic;
	var backgroundPosition : Dynamic;
	var backgroundRepeat : Dynamic;

	// Border
	var border : Dynamic;
	var borderBottom : Dynamic;
	var borderLeft : Dynamic;
	var borderRight : Dynamic;
	var borderTop : Dynamic;
	var borderBottomColor : Dynamic;
	var borderLeftColor : Dynamic;
	var borderRightColor : Dynamic;
	var borderTopColor : Dynamic;
	var borderBottomStyle : Dynamic;
	var borderLeftStyle : Dynamic;
	var borderRightStyle : Dynamic;
	var borderTopStyle : Dynamic;
	var borderBottomWidth : Dynamic;
	var borderLeftWidth : Dynamic;
	var borderRightWidth : Dynamic;
	var borderTopWidth : Dynamic;
	var borderColor : Dynamic;
	var borderStyle : String;
	var borderWidth : Dynamic;


	var margin : Dynamic;
	var marginBottom : Dynamic;
	var marginLeft : Dynamic;
	var marginRight : Dynamic;
	var marginTop : Dynamic;

	var outline : Dynamic;
	var outlineColor : Dynamic;
	var outlineStyle : Dynamic;
	var outlineWidth : Dynamic;
	var padding : Dynamic;
	var paddingBottom : Dynamic;
	var paddingLeft : Dynamic;
	var paddingRight : Dynamic;
	var paddingTop : Dynamic;

	var clear : String;
	var clip : Dynamic;
	var clipBottom : Dynamic;
	var clipLeft : Dynamic;
	var clipRight : Dynamic;
	var clipTop : Dynamic;

	var content : Dynamic;
	var counterIncrement : Dynamic;
	var counterReset : Dynamic;
	var cssFloat : Dynamic;
	var cursor : Dynamic;
	var direction : Dynamic;
	var display : Dynamic;

	var markerOffset : Dynamic;
	var marks : Dynamic;
	var maxHeight : Dynamic;
	var maxWidth : Dynamic;
	var minHeight : Dynamic;
	var minWidth : Dynamic;
	var overflow : Dynamic;
	var overflowX : Dynamic;
	var overflowY : Dynamic;
	var styleFloat : Dynamic;
	var verticalAlign : Dynamic;
	var visibility : Dynamic;
	var zoom : Dynamic;

	var listStyle : Dynamic;
	var listStyleImage : Dynamic;
	var listStylePosition : Dynamic;
	var listStyleType : String;

	var cssText : Dynamic;

	var color : Dynamic;
	var font : Dynamic;
	var fontFamily : Dynamic;
	var fontSize : Dynamic;
	var fontSizeAdjust : Dynamic;
	var fontStretch : Dynamic;
	var fontStyle : Dynamic;
	var fontVariant : Dynamic;
	var fontWeight : Dynamic;
	var letterSpacing : Dynamic;
	var lineBreak : Dynamic;
	var lineHeight : Dynamic;
	var quotes : Dynamic;
	var rubyAlign : Dynamic;
	var rubyOverhang : Dynamic;
	var rubyPosition : Dynamic;
	var textAlign : Dynamic;
	var textAlignLast : Dynamic;
	var textAutospace : Dynamic;
	var textDecoration : Dynamic;
	var textDecorationBlink : Dynamic;
	var textDecorationLineThrough : Dynamic;
	var textDecorationLineNone : Dynamic;
	var textDecorationLineOverline : Dynamic;
	var textDecorationLineUnderline : Dynamic;
	var textIndent : Dynamic;
	var textJustify : Dynamic;
	var textJustifyTrim : Dynamic;
	var textKashidaSpace : Dynamic;
	var textOverflow : Dynamic;
	var textShadow : Dynamic;
	var textTransform : Dynamic;
	var textUnderlinePosition : Dynamic;
	var unicodeBidi : Dynamic;
	var whiteSpace : Dynamic;
	var wordBreak : Dynamic;
	var wordSpacing : Dynamic;
	var wordWrap : Dynamic;
	var writingMode : Dynamic;

}
