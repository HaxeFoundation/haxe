package python.lib.io;

import python.lib.io.BufferedIOBase;
import python.lib.io.TextIOBase;
import python.Syntax;
import python.KwArgs;

typedef TextIOWrapperOptions = {
	?encoding : String,
	?errors : String,
	?newline : String,
	?line_buffering : Bool,
	?write_through : Bool
};

@:pythonImport("io", "TextIOWrapper")
extern class TextIOWrapper extends TextIOBase
{
	public function new (buffer:BufferedIOBase, ?options:KwArgs<TextIOWrapperOptions>):Void;

	public var line_buffering : Bool;
}