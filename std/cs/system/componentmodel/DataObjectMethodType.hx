package cs.system.componentmodel;

/** Identifies the type of data operation performed by a method, as specified by the  applied to the method. */
@:native("System.ComponentModel.DataObjectMethodType")
extern enum DataObjectMethodType {
	Delete;
	Fill;
	Insert;
	Select;
	Update;
}
