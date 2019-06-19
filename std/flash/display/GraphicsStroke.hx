package flash.display;

extern final class GraphicsStroke implements IGraphicsData implements IGraphicsStroke {
	@:flash.property var caps(get,set) : CapsStyle;
	var fill : IGraphicsFill;
	@:flash.property var joints(get,set) : JointStyle;
	var miterLimit : Float;
	var pixelHinting : Bool;
	@:flash.property var scaleMode(get,set) : LineScaleMode;
	var thickness : Float;
	function new(thickness : Float = 0./*NaN*/, pixelHinting : Bool = false, ?scaleMode : LineScaleMode, ?caps : CapsStyle, ?joints : JointStyle, miterLimit : Float = 3, ?fill : IGraphicsFill) : Void;
	private function get_caps() : CapsStyle;
	private function get_joints() : JointStyle;
	private function get_scaleMode() : LineScaleMode;
	private function set_caps(value : CapsStyle) : CapsStyle;
	private function set_joints(value : JointStyle) : JointStyle;
	private function set_scaleMode(value : LineScaleMode) : LineScaleMode;
}
