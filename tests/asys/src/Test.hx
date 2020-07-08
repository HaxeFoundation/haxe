/**
	Base class for asys tests
**/
class Test extends utest.Test {
	static final __systemName = Sys.systemName();

	var isWindows(get,never):Bool;
	function get_isWindows():Bool {
		return __systemName == 'Windows';
	}
}