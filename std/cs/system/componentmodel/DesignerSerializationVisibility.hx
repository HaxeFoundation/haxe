package cs.system.componentmodel;

/** Specifies the visibility a property has to the design-time serializer. */
@:native("System.ComponentModel.DesignerSerializationVisibility")
extern enum DesignerSerializationVisibility {
	Content;
	Hidden;
	Visible;
}
