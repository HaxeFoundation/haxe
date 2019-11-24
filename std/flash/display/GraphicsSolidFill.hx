package flash.display;

extern final class GraphicsSolidFill implements IGraphicsData implements IGraphicsFill {
	var alpha : Float;
	var color : UInt;
	function new(color : UInt = 0, alpha : Float = 1) : Void;
}
