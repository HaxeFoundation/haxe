package flash.display3D;

@:native("flash.display3D.Context3DStencilAction") extern enum abstract Context3DStencilAction(String) {
	var DECREMENT_SATURATE;
	var DECREMENT_WRAP;
	var INCREMENT_SATURATE;
	var INCREMENT_WRAP;
	var INVERT;
	var KEEP;
	var SET;
	var ZERO;
}
