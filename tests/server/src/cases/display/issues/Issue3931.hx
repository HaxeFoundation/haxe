package cases.display.issues;

class Issue3931 extends DisplayTestCase {
	/**
		@:access(pack.{-1-}
		class Main1 {
			static function main() { }
		}
	**/
	function testPackageCompletion(_) {
		vfs.putContent("pack/A.hx", getTemplate("issues/Issue3931/pack/A.hx"));
		vfs.putContent("pack/B.hx", getTemplate("issues/Issue3931/pack/B.hx"));
		var items = fields(1);
		Assert.isTrue(hasPath(items, "A"));
		Assert.isTrue(hasPath(items, "B"));
	}

	/**
		@:access(pack.A.{-1-}
		class Main2 {
			static function main() { }
		}
	**/
	function testTypeFieldCompletion(_) {
		vfs.putContent("pack/A.hx", getTemplate("issues/Issue3931/pack/A.hx"));
		vfs.putContent("pack/B.hx", getTemplate("issues/Issue3931/pack/B.hx"));
		var items = fields(1);
		// pack.A has class A and class C
		Assert.isTrue(hasPath(items, "A") || hasPath(items, "C"));
	}
}
