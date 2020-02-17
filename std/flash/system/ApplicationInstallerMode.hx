package flash.system;

@:native("flash.system.ApplicationInstallerMode") extern enum abstract ApplicationInstallerMode(String) {
	var INSTALL_ONLY;
	var INSTALL_WITH_SHORTCUTS;
	var SHORTCUTS_ONLY;
}
