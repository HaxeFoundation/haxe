package rust.vm;
import rust.*;
@:native("run.ProgramOutput") extern class ProgramOutput {
	public var status(default, null):Int;
	public var out(default, null):String;
	public var err(default, null):String;
}