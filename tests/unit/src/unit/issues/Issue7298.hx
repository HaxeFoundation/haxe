package unit.issues;

class Issue7298 extends unit.Test {
	static var _pageIndex = 0;
	function test() {
		var currentPageIndex = _pageIndex;
    	var r = _updatePageIndexSelection(++currentPageIndex);
		eq(0, r);
	}

	static function _updatePageIndexSelection(v:Int) return _pageIndex;
}