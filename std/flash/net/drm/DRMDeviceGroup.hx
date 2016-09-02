package flash.net.drm;

@:final extern class DRMDeviceGroup {
	var authenticationMethod(default,never) : String;
	var domain(default,never) : String;
	var name : String;
	var serverURL(default,never) : String;
	function new() : Void;
}
