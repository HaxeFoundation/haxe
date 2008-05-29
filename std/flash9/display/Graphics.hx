package flash.display;

extern class Graphics {
	function new() : Void;
	function beginBitmapFill(bitmap : BitmapData, ?matrix : flash.geom.Matrix, ?repeat : Bool, ?smooth : Bool) : Void;
	function beginFill(color : UInt, ?alpha : Float) : Void;
	function beginGradientFill(type : GradientType, colors : Array<UInt>, alphas : Array<Dynamic>, ratios : Array<Dynamic>, ?matrix : flash.geom.Matrix, ?spreadMethod : SpreadMethod, ?interpolationMethod : InterpolationMethod, ?focalPointRatio : Float) : Void;
	function clear() : Void;
	function curveTo(controlX : Float, controlY : Float, anchorX : Float, anchorY : Float) : Void;
	function drawCircle(x : Float, y : Float, radius : Float) : Void;
	function drawEllipse(x : Float, y : Float, width : Float, height : Float) : Void;
	function drawRect(x : Float, y : Float, width : Float, height : Float) : Void;
	function drawRoundRect(x : Float, y : Float, width : Float, height : Float, ellipseWidth : Float, ?ellipseHeight : Float) : Void;
	function drawRoundRectComplex(x : Float, y : Float, width : Float, height : Float, topLeftRadius : Float, topRightRadius : Float, bottomLeftRadius : Float, bottomRightRadius : Float) : Void;
	function endFill() : Void;
	function lineGradientStyle(type : GradientType, colors : Array<UInt>, alphas : Array<Dynamic>, ratios : Array<Dynamic>, ?matrix : flash.geom.Matrix, ?spreadMethod : SpreadMethod, ?interpolationMethod : InterpolationMethod, ?focalPointRatio : Float) : Void;
	function lineStyle(?thickness : Float, ?color : UInt, ?alpha : Float, ?pixelHinting : Bool, ?scaleMode : LineScaleMode, ?caps : CapsStyle, ?joints : JointStyle, ?miterLimit : Float) : Void;
	function lineTo(x : Float, y : Float) : Void;
	function moveTo(x : Float, y : Float) : Void;

	#if flash10
	function beginShaderFill( ?shader : Shader, ?matrix : flash.geom.Matrix ) : Void;
	function copyFrom( sourceGraphics : Graphics ) : Void;
	function drawGraphicsData( graphicsData : flash.Vector<IGraphicsData> ) : Void;
	function drawPath( ?commands : flash.Vector<Int>, ?data : flash.Vector<Float>, ?winding : GraphicsPathWinding ) : Dynamic;
	function drawTriangles( ?vertices : flash.Vector<Float>, ?indices : flash.Vector<Int>, ?uvtData : flash.Vector<Float>, ?culling : TriangleCulling ) : Dynamic;
	function lineBitmapStyle( ?bitmap : BitmapData, ?matrix : flash.geom.Matrix, ?repeat : Bool, ?smooth : Bool ) : Void;
	#end
}
