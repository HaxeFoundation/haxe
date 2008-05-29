package flash.display;

extern class GraphicsStroke implements IGraphicsStroke, implements IGraphicsData {
	function new(?_thickness : Float, ?_pixelHinting : Bool, ?_scaleMode : String, ?_caps : String, ?_joints : String, ?_miterLimit : Float, ?_fill : flash.display.IGraphicsFill) : Void;
	var caps : String;
	var fill : flash.display.IGraphicsFill;
	var joints : String;
	var miterLimit : Float;
	var pixelHinting : Bool;
	var scaleMode : String;
	var thickness : Float;
}
