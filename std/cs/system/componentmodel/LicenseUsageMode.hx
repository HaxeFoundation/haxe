package cs.system.componentmodel;

/** Specifies when the  can be used. */
@:native("System.ComponentModel.LicenseUsageMode")
extern enum abstract LicenseUsageMode(Int) {
	var Designtime = 1;
	var Runtime = 0;
}
