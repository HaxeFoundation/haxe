package issues;

class Issue7237 {

	@:js('
		var f = function(a) {
			var b = a;
			return b;
		};
		var f2 = function(a) {
			var b = a;
			return b;
		};
	')
	@:analyzer(ignore)
	static function main() {
		var f = function(a) {
			var b = a;
			return b;
		}
		var f2 = function(a) {
			var b = a;
			return b;
		}
	}
}
