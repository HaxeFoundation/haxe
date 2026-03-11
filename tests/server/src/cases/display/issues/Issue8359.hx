package cases.display.issues;

class Issue8359 extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				var active = false;
				var callback: ()->Void;
				callback = function original(){
					if( active ){ return; }
					active = true;
					callback = function(){
						active = false;
						callback = original;
					};
				};
				callback();
				callback();
			}
		}
	**/
	function test(_) {
		var files = runHaxeJson([], DisplayMethods.Diagnostics, {file: file});
		final diags:Array<haxe.display.Diagnostic<Any>> = files != null && files.length > 0 ? cast files[0].diagnostics : [];
		Assert.equals(0, diags.length);
	}
}
