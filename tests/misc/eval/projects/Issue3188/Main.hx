import haxe.macro.Context;

macro function init():Void {
	Context.parseInlineString("null", Context.makePosition( { file: "external.txt", min: 14, max: 18, }));
	Context.warning("Correct line number", Context.makePosition( { file: "external.txt", min: 24, max: 27, }));
	Context.warning("Correct line number", Context.makePosition( { file: "./external.txt", min: 24, max: 27, }));
}
