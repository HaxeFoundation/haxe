package cases;

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
	function test() {
		arrayEq([], diagnostics());
	}
}
