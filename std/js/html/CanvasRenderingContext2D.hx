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

/** The bulk of the operations available at present with <code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/canvas">&lt;canvas&gt;</a></code>
 are available through this interface, returned by a call to <code>getContext()</code> on the <code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/canvas">&lt;canvas&gt;</a></code>
 element, with "2d" as its argument.<br><br>
Documentation for this class was provided by <a href="https://developer.mozilla.org/en/DOM/CanvasRenderingContext2D">MDN</a>. */
@:native("CanvasRenderingContext2D")
extern class CanvasRenderingContext2D extends CanvasRenderingContext
{
	var backingStorePixelRatio (default,null) : Float;

	/** Color or style to use inside shapes. Default <code>#000</code> (black). */
	var fillStyle : Dynamic;

	/** Default value <code>10px sans-serif</code>. */
	var font : String;

	/** Alpha value that is applied to shapes and images before they are composited onto the canvas. Default <code>1.0</code> (opaque). */
	var globalAlpha : Float;

	/** With <code>globalAplpha</code> applied this sets how shapes and images are drawn onto the existing bitmap. Possible values: <ul> <li><code>source-atop</code></li> <li><code>source-in</code></li> <li><code>source-out</code></li> <li><code>source-over</code> (default)</li> <li><code>destination-atop</code></li> <li><code>destination-in</code></li> <li><code>destination-out</code></li> <li><code>destination-over</code></li> <li><code>lighter</code></li> <li><code>xor</code></li> </ul> */
	var globalCompositeOperation : String;

	var imageSmoothingEnabled : Bool;

	/** Type of endings on the end of lines. Possible values: <code>butt</code> (default), <code>round</code>, <code>square</code> */
	var lineCap : String;

	var lineDash : Array<Dynamic>;

	var lineDashOffset : Float;

	/** Defines the type of corners where two lines meet. Possible values: <code>round</code>, <code>bevel</code>, <code>miter</code> (default) */
	var lineJoin : String;

	/** Width of lines. Default <code>1.0</code> */
	var lineWidth : Float;

	/** Default <code>10</code>. */
	var miterLimit : Float;

	/** Specifies the blurring effect. Default <code>0</code> */
	var shadowBlur : Float;

	/** Color of the shadow. Default fully-transparent black. */
	var shadowColor : String;

	/** Horizontal distance the shadow will be offset. Default 0. */
	var shadowOffsetX : Float;

	/** Vertical distance the shadow will be offset. Default 0. */
	var shadowOffsetY : Float;

	/** Color or style to use for the lines around shapes. Default <code>#000</code> (black). */
	var strokeStyle : Dynamic;

	/** Possible values: <code>start</code> (default), <code>end</code>, <code>left</code>, <code>right</code> or <code>center</code>. */
	var textAlign : String;

	var textBaseline : String;

	function arc( x : Float, y : Float, radius : Float, startAngle : Float, endAngle : Float, anticlockwise : Bool ) : Void;

	function arcTo( x1 : Float, y1 : Float, x2 : Float, y2 : Float, radius : Float ) : Void;

	function beginPath() : Void;

	function bezierCurveTo( cp1x : Float, cp1y : Float, cp2x : Float, cp2y : Float, x : Float, y : Float ) : Void;

	function clearRect( x : Float, y : Float, width : Float, height : Float ) : Void;

	function clearShadow() : Void;

	function clip() : Void;

	function closePath() : Void;

	/** Throws DOMException. */
	@:overload( function( imagedata : ImageData ) :ImageData {} )
	function createImageData( sw : Float, sh : Float ) : ImageData;

	function createLinearGradient( x0 : Float, y0 : Float, x1 : Float, y1 : Float ) : CanvasGradient;

	/** <div id="section_31"><span id="Parameters_10"></span><h6 class="editable">Parameters</h6>
<dl> <dt><code>image</code></dt> <dd>A DOM <code><a rel="custom" href="https://developer.mozilla.org/en/DOM/element">element</a></code>
 to use as the source image for the pattern. This can be any element, although typically you'll use an <code><a rel="internal" href="https://developer.mozilla.org/Article_not_found?uri=en/DOM/Image" class="new">Image</a></code>
 or <code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/canvas">&lt;canvas&gt;</a></code>
.</dd> <dt><code>repetition</code></dt> <dd>?</dd>
</dl>
</div><div id="section_32"><span id="Return_value_3"></span><h6 class="editable">Return value</h6>
<p>A new DOM canvas pattern object for use in pattern-based operations.</p>
</div><div id="section_33"><span id="Exceptions_thrown"></span><h6 class="editable">Exceptions thrown</h6>
<dl> <dt><code>NS_ERROR_DOM_INVALID_STATE_ERR</code> 
<span title="(Firefox 10.0 / Thunderbird 10.0)
">Requires Gecko 10.0</span>
</dt> <dd>The specified <code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/canvas">&lt;canvas&gt;</a></code>
 element for the <code>image</code> parameter is zero-sized (that is, one or both of its dimensions are 0 pixels).</dd>
</dl>
</div> Throws DOMException. */
	@:overload( function( canvas : CanvasElement, repetitionType : String ) :CanvasPattern {} )
	function createPattern( image : ImageElement, repetitionType : String ) : CanvasPattern;

	function createRadialGradient( x0 : Float, y0 : Float, r0 : Float, x1 : Float, y1 : Float, r1 : Float ) : CanvasGradient;

	/** <p>Draws the specified image. This method is available in multiple formats, providing a great deal of flexibility in its use.</p>

<div id="section_41"><span id="Parameters_13"></span><h6 class="editable">Parameters</h6>
<dl> <dt><code>image</code></dt> <dd>An element to draw into the context; the specification permits any image element (that is, <code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/img">&lt;img&gt;</a></code>
, <code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/canvas">&lt;canvas&gt;</a></code>
, and <code><a rel="custom" href="https://developer.mozilla.org/en/HTML/Element/video">&lt;video&gt;</a></code>
). Some browsers, including Firefox, let you use any arbitrary element.</dd> <dt><code>dx</code></dt> <dd>The X coordinate in the destination canvas at which to place the top-left corner of the source <code>image</code>.</dd> <dt><code>dy</code></dt> <dd>The Y coordinate in the destination canvas at which to place the top-left corner of the source <code>image</code>.</dd> <dt><code>dw</code></dt> <dd>The width to draw the <code>image</code> in the destination canvas. This allows scaling of the drawn image. If not specified, the image is not scaled in width when drawn.</dd> <dt><code>dh</code></dt> <dd>The height to draw the <code>image</code> in the destination canvas. This allows scaling of the drawn image. If not specified, the image is not scaled in height when drawn.</dd> <dt><code>sx</code></dt> <dd>The X coordinate of the top left corner of the sub-rectangle of the source image to draw into the destination context.</dd> <dt><code>sy</code></dt> <dd>The Y coordinate of the top left corner of the sub-rectangle of the source image to draw into the destination context.</dd> <dt><code>sw</code></dt> <dd>The width of the sub-rectangle of the source image to draw into the destination context. If not specified, the entire rectangle from the coordinates specified by <code>sx</code> and <code>sy</code> to the bottom-right corner of the image is used. If you specify a negative value, the image is flipped horizontally when drawn.</dd> <dt><code>sh</code></dt> <dd>The height of the sub-rectangle of the source image to draw into the destination context. If you specify a negative value, the image is flipped vertically when drawn.</dd>
</dl>
<p>The diagram below illustrates the meanings of the various parameters.</p>
<p><img alt="drawImage.png" class="internal default" src="https://developer.mozilla.org/@api/deki/files/5494/=drawImage.png"></p>
</div><div id="section_42"><span id="Exceptions_thrown_2"></span><h6 class="editable">Exceptions thrown</h6>
<dl> <dt><code>INDEX_SIZE_ERR</code></dt> <dd>If the canvas or source rectangle width or height is zero.</dd> <dt><code>INVALID_STATE_ERR</code></dt> <dd>The image has no image data.</dd> <dt><code>TYPE_MISMATCH_ERR</code></dt> <dd>The specified source element isn't supported. This is not thrown by Firefox, since any element may be used as a source image.</dd>
</dl>
</div><div id="section_43"><span id="Compatibility_notes"></span><h6 class="editable">Compatibility notes</h6>
<ul> <li>Prior to Gecko 7.0 (Firefox 7.0 / Thunderbird 7.0 / SeaMonkey 2.4)
, Firefox threw an exception if any of the coordinate values was non-finite or zero. As per the specification, this no longer happens.</li> <li>Support for flipping the image by using negative values for <code>sw</code> and <code>sh</code> was added in Gecko 5.0 (Firefox 5.0 / Thunderbird 5.0 / SeaMonkey 2.2)
.</li> <li>Gecko 9.0 (Firefox 9.0 / Thunderbird 9.0 / SeaMonkey 2.6)
 now correctly supports CORS for drawing images across domains without <a title="en/CORS_Enabled_Image#What_is_a_.22tainted.22_canvas.3F" rel="internal" href="https://developer.mozilla.org/en/CORS_Enabled_Image#What_is_a_.22tainted.22_canvas.3F">tainting the canvas</a>.</li>
</ul>
</div> Throws DOMException. */
	@:overload( function( image : ImageElement, x : Float, y : Float ) :Void {} )
	@:overload( function( image : ImageElement, x : Float, y : Float, width : Float, height : Float ) :Void {} )
	@:overload( function( image : ImageElement, sx : Float, sy : Float, sw : Float, sh : Float, dx : Float, dy : Float, dw : Float, dh : Float ) :Void {} )
	@:overload( function( canvas : CanvasElement, x : Float, y : Float ) :Void {} )
	@:overload( function( canvas : CanvasElement, x : Float, y : Float, width : Float, height : Float ) :Void {} )
	@:overload( function( canvas : CanvasElement, sx : Float, sy : Float, sw : Float, sh : Float, dx : Float, dy : Float, dw : Float, dh : Float ) :Void {} )
	@:overload( function( video : VideoElement, x : Float, y : Float ) :Void {} )
	@:overload( function( video : VideoElement, x : Float, y : Float, width : Float, height : Float ) :Void {} )
	function drawImage( video : VideoElement, sx : Float, sy : Float, sw : Float, sh : Float, dx : Float, dy : Float, dw : Float, dh : Float ) : Void;

	function drawImageFromRect( image : ImageElement, ?sx : Float, ?sy : Float, ?sw : Float, ?sh : Float, ?dx : Float, ?dy : Float, ?dw : Float, ?dh : Float, ?compositeOperation : String ) : Void;

	function fill() : Void;

	function fillRect( x : Float, y : Float, width : Float, height : Float ) : Void;

	function fillText( text : String, x : Float, y : Float, ?maxWidth : Float ) : Void;

	function getImageData( sx : Float, sy : Float, sw : Float, sh : Float ) : ImageData;

	function getImageDataHD( sx : Float, sy : Float, sw : Float, sh : Float ) : ImageData;

	function getLineDash() : Array<Float>;

	function isPointInPath( x : Float, y : Float ) : Bool;

	function lineTo( x : Float, y : Float ) : Void;

	function measureText( text : String ) : TextMetrics;

	function moveTo( x : Float, y : Float ) : Void;

	/** <h6 class="editable">Compatibility notes</h6>
<ul> <li>Starting in Gecko 10.0 (Firefox 10.0 / Thunderbird 10.0)
, non-finite values to any of these parameters causes the call to putImageData() to be silently ignored, rather than throwing an exception.</li>
</ul> Throws DOMException. */
	@:overload( function( imagedata : ImageData, dx : Float, dy : Float ) :Void {} )
	function putImageData( imagedata : ImageData, dx : Float, dy : Float, dirtyX : Float, dirtyY : Float, dirtyWidth : Float, dirtyHeight : Float ) : Void;

	/** Throws DOMException. */
	@:overload( function( imagedata : ImageData, dx : Float, dy : Float ) :Void {} )
	function putImageDataHD( imagedata : ImageData, dx : Float, dy : Float, dirtyX : Float, dirtyY : Float, dirtyWidth : Float, dirtyHeight : Float ) : Void;

	function quadraticCurveTo( cpx : Float, cpy : Float, x : Float, y : Float ) : Void;

	function rect( x : Float, y : Float, width : Float, height : Float ) : Void;

	function restore() : Void;

	function rotate( angle : Float ) : Void;

	function save() : Void;

	function scale( sx : Float, sy : Float ) : Void;

	function setAlpha( alpha : Float ) : Void;

	function setCompositeOperation( compositeOperation : String ) : Void;

	@:overload( function( color : String, ?alpha : Float ) :Void {} )
	@:overload( function( grayLevel : Float, ?alpha : Float ) :Void {} )
	@:overload( function( r : Float, g : Float, b : Float, a : Float ) :Void {} )
	function setFillColor( c : Float, m : Float, y : Float, k : Float, a : Float ) : Void;

	function setLineCap( cap : String ) : Void;

	function setLineDash( dash : Array<Float> ) : Void;

	function setLineJoin( join : String ) : Void;

	function setLineWidth( width : Float ) : Void;

	function setMiterLimit( limit : Float ) : Void;

	@:overload( function( width : Float, height : Float, blur : Float, ?color : String, ?alpha : Float ) :Void {} )
	@:overload( function( width : Float, height : Float, blur : Float, grayLevel : Float, ?alpha : Float ) :Void {} )
	@:overload( function( width : Float, height : Float, blur : Float, r : Float, g : Float, b : Float, a : Float ) :Void {} )
	function setShadow( width : Float, height : Float, blur : Float, c : Float, m : Float, y : Float, k : Float, a : Float ) : Void;

	@:overload( function( color : String, ?alpha : Float ) :Void {} )
	@:overload( function( grayLevel : Float, ?alpha : Float ) :Void {} )
	@:overload( function( r : Float, g : Float, b : Float, a : Float ) :Void {} )
	function setStrokeColor( c : Float, m : Float, y : Float, k : Float, a : Float ) : Void;

	function setTransform( m11 : Float, m12 : Float, m21 : Float, m22 : Float, dx : Float, dy : Float ) : Void;

	function stroke() : Void;

	function strokeRect( x : Float, y : Float, width : Float, height : Float, ?lineWidth : Float ) : Void;

	function strokeText( text : String, x : Float, y : Float, ?maxWidth : Float ) : Void;

	function transform( m11 : Float, m12 : Float, m21 : Float, m22 : Float, dx : Float, dy : Float ) : Void;

	function translate( tx : Float, ty : Float ) : Void;

}
