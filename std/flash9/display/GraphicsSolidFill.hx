package flash.display;

extern class GraphicsSolidFill implements IGraphicsFill, implements IGraphicsData {
	function new(?color : UInt, ?alpha : Float) : Void;
	var alpha : Float;
	var color : UInt;
}
