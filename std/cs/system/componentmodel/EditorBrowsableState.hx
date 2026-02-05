package cs.system.componentmodel;

/** Specifies the browsable state of a property or method from within an editor. */
@:native("System.ComponentModel.EditorBrowsableState")
extern enum abstract EditorBrowsableState(Int) {
	var Advanced = 2;
	var Always = 0;
	var Never = 1;
}
