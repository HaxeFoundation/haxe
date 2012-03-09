package flash.display3D;

@:fakeEnum(String) extern enum Context3DStencilAction {
	DECREMENT_SATURATE;
	DECREMENT_WRAP;
	INCREMENT_SATURATE;
	INCREMENT_WRAP;
	INVERT;
	KEEP;
	SET;
	ZERO;
}
