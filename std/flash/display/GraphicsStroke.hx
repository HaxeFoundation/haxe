package flash.display;

@:final extern class GraphicsStroke implements IGraphicsData implements IGraphicsStroke {
	var caps : CapsStyle;
	var fill : IGraphicsFill;
	var joints : JointStyle;
	var miterLimit : Float;
	var pixelHinting : Bool;
	var scaleMode : LineScaleMode;
	var thickness : Float;
	function new(thickness : Float = 0./*NaN*/, pixelHinting : Bool = false, ?scaleMode : LineScaleMode, ?caps : CapsStyle, ?joints : JointStyle, miterLimit : Float = 3, ?fill : IGraphicsFill) : Void;
}
