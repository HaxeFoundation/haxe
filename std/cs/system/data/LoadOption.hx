package cs.system.data;

/** Controls how the values from the data source will be applied to existing rows when using the  or  method. */
@:native("System.Data.LoadOption")
extern enum LoadOption {
	OverwriteChanges;
	PreserveChanges;
	Upsert;
}
