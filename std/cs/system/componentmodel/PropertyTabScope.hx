package cs.system.componentmodel;

/** Defines identifiers that indicate the persistence scope of a tab in the Properties window. */
@:native("System.ComponentModel.PropertyTabScope")
extern enum PropertyTabScope {
	Component;
	Document;
	Global;
	Static;
}
