typedef PrivateVariationsType = {
	var privateBoth(private get, private set):Int;
	var privateGetSet(private get, set):Int;
	var getPrivateSet(get, private set):Int;
	var defaultPrivateSet(default, private set):Int;
	var getNever(get, never):Int;
}

class PrivateVariations {
	public function new() {}
	var privateBoth(private get, private set):Int;
	public var privateGetSet(private get, set):Int;
	public var getPrivateSet(get, private set):Int;
	public var defaultPrivateSet(default, private set):Int;

	static var sprivateBoth(private get, private set):Int;
	static public var sprivateGetSet(private get, set):Int;
	static public var sgetPrivateSet(get, private set):Int;
	static public var sdefaultPrivateSet(default, private set):Int;

	function set_privateBoth(v):Int return 0;
	function get_privateBoth():Int return 0;
	function set_privateGetSet(v):Int return 0;
	function get_privateGetSet():Int return 0;
	function set_getPrivateSet(v):Int return 0;
	function get_getPrivateSet():Int return 0;
	function set_defaultPrivateSet(v):Int return 0;

	static function set_sprivateBoth(v):Int return 0;
	static function get_sprivateBoth():Int return 0;
	static function set_sprivateGetSet(v):Int return 0;
	static function get_sprivateGetSet():Int return 0;
	static function set_sgetPrivateSet(v):Int return 0;
	static function get_sgetPrivateSet():Int return 0;
	static function set_sdefaultPrivateSet(v):Int return 0;
}

class CheckVariations {
	static function main() {
		final type:PrivateVariationsType = null;
		type.getNever = 1; // err
		type.getPrivateSet;
		type.getPrivateSet = 1; // err
		type.getPrivateSet += 1; // err
		type.getPrivateSet++; // err
		type.privateGetSet; // err
		type.privateGetSet = 1;
		type.privateGetSet += 1; // err (reading)
		type.privateGetSet++; // err (reading)
		type.privateBoth; // err
		type.privateBoth = 1; // err
		type.privateBoth += 1; // err
		type.privateBoth++; // err
		@:privateAccess type.privateBoth;
		@:privateAccess type.privateBoth = 1;
		@:privateAccess type.privateBoth += 1;
		@:privateAccess type.privateBoth++;
		@:bypassAccessor type.privateBoth; // err
		@:bypassAccessor type.privateBoth = 1; // err
		@:bypassAccessor type.privateBoth += 1; // err
		@:bypassAccessor type.privateBoth++; // err

		type.defaultPrivateSet;
		type.defaultPrivateSet = 1; // err
		type.defaultPrivateSet += 1; // err
		type.defaultPrivateSet++; // err
		@:privateAccess type.defaultPrivateSet = 1;
		@:privateAccess type.defaultPrivateSet += 1;
		@:privateAccess type.defaultPrivateSet++;
		@:bypassAccessor type.defaultPrivateSet = 1;
		@:bypassAccessor type.defaultPrivateSet += 1;
		@:bypassAccessor type.defaultPrivateSet++;

		final vars = new PrivateVariations();
		vars.getPrivateSet;
		vars.getPrivateSet = 1; // err
		vars.getPrivateSet += 1; // err
		vars.getPrivateSet++; // err
		vars.privateGetSet; // err
		vars.privateGetSet = 1;
		vars.privateGetSet += 1; // err (reading)
		vars.privateGetSet++; // err (reading)
		vars.privateBoth; // err
		vars.privateBoth = 1; // err
		vars.privateBoth += 1; // err
		vars.privateBoth++; // err
		@:privateAccess vars.privateBoth;
		@:privateAccess vars.privateBoth = 1;
		@:privateAccess vars.privateBoth += 1;
		@:privateAccess vars.privateBoth++;
		vars.defaultPrivateSet;
		vars.defaultPrivateSet = 1; // err
		vars.defaultPrivateSet += 1; // err
		vars.defaultPrivateSet++; // err
		@:privateAccess vars.defaultPrivateSet = 1;
		@:privateAccess vars.defaultPrivateSet += 1;
		@:privateAccess vars.defaultPrivateSet++;
		@:bypassAccessor vars.defaultPrivateSet = 1;
		@:bypassAccessor vars.defaultPrivateSet += 1;
		@:bypassAccessor vars.defaultPrivateSet++;

		PrivateVariations.sgetPrivateSet;
		PrivateVariations.sgetPrivateSet = 1; // err
		PrivateVariations.sgetPrivateSet += 1; // err
		PrivateVariations.sgetPrivateSet++; // err
		PrivateVariations.sprivateGetSet; // err
		PrivateVariations.sprivateGetSet = 1;
		PrivateVariations.sprivateGetSet += 1; // err (reading)
		PrivateVariations.sprivateGetSet++; // err (reading)
		PrivateVariations.sprivateBoth; // err
		PrivateVariations.sprivateBoth = 1; // err
		PrivateVariations.sprivateBoth += 1; // err
		PrivateVariations.sprivateBoth++; // err
		@:privateAccess PrivateVariations.sprivateBoth;
		@:privateAccess PrivateVariations.sprivateBoth = 1;
		@:privateAccess PrivateVariations.sprivateBoth += 1;
		@:privateAccess PrivateVariations.sprivateBoth++;
		PrivateVariations.sdefaultPrivateSet;
		PrivateVariations.sdefaultPrivateSet = 1; // err
		PrivateVariations.sdefaultPrivateSet += 1; // err
		PrivateVariations.sdefaultPrivateSet++; // err
		@:privateAccess PrivateVariations.sdefaultPrivateSet = 1;
		@:privateAccess PrivateVariations.sdefaultPrivateSet += 1;
		@:privateAccess PrivateVariations.sdefaultPrivateSet++;
		@:bypassAccessor PrivateVariations.sdefaultPrivateSet = 1;
		@:bypassAccessor PrivateVariations.sdefaultPrivateSet += 1;
		@:bypassAccessor PrivateVariations.sdefaultPrivateSet++;
	}
}
