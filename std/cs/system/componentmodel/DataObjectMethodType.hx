package cs.system.componentmodel;

/** Identifies the type of data operation performed by a method, as specified by the  applied to the method. */
@:native("System.ComponentModel.DataObjectMethodType")
extern enum abstract DataObjectMethodType(Int) {
	var Delete = 4;
	var Fill = 0;
	var Insert = 3;
	var Select = 1;
	var Update = 2;
}
