package unit.issues;

class Issue7536 extends unit.Test {
	function test() {
      var s:String = null;
      var url:Dynamic = { query: s }
      t(url.query == null);
	}
}