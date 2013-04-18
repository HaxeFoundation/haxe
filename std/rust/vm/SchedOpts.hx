package rust.vm;

@:native("task.SchedOpts") extern class SchedOpts {
	public var mode(default, null): SchedMode;
	public var foreign_stack_size(default, null):Null<Int>;
}