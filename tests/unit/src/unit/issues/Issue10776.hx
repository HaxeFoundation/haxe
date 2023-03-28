package unit.issues;

import utest.Assert;

@:forward
private abstract DialogConfig(DialogConfigData) from DialogConfigData {
	@:from static function fromRenderResult(r:RenderResult):DialogConfig
		return {priority: 0, ui: r};
}

private typedef DialogConfigData = {
	final priority:Int;
	final ui:DialogUi;
}

@:callable
private abstract DialogUi((close:() -> Void)->RenderResult) from (close:() -> Void)->RenderResult {
	inline function new(f)
		this = f;

	@:from static function fromRenderResult(r:RenderResult)
		return new DialogUi(_ -> r);
}

private abstract RenderResult(String) to String from String {}

class Issue10776 extends Test {
	function test() {
		var cfg:DialogConfig = {
			priority: 0,
			ui: (null : RenderResult),
		};
		Assert.pass();
	}
}
