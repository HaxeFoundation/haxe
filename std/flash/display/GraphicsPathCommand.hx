package flash.display;

extern class GraphicsPathCommand {
	public static inline var LINE_TO = 2;
	public static inline var MOVE_TO = 1;
	public static inline var CURVE_TO = 3;
	@:require(flash11) public static inline var CUBIC_CURVE_TO = 6;
	public static inline var WIDE_LINE_TO = 5;
	public static inline var WIDE_MOVE_TO = 4;
	public static inline var NO_OP = 0;
}
