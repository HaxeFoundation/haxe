package cs.system.componentmodel;

/** Specifies the visibility a property has to the design-time serializer. */
@:native("System.ComponentModel.DesignerSerializationVisibility")
extern enum abstract DesignerSerializationVisibility(Int) {
	var Content = 2;
	var Hidden = 0;
	var Visible = 1;
}
