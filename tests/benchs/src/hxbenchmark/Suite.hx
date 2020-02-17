package hxbenchmark;

import haxe.macro.Expr;

class Suite {
	public var maxTimePerCase:Float;
	final name:String;
	final cases:Array<() -> CaseResult>;

	public function new(name:String, ?maxTimePerCase = .5) {
		this.name = name;
		this.maxTimePerCase = maxTimePerCase;
		cases = [];
	}

	macro public function add(eThis:Expr, name:String, expr:Expr, ?exprInit:Expr) {
		if (exprInit == null) {
			exprInit = macro {};
		}
		return macro @:privateAccess {
			var f = function() {
				var currentTime = haxe.Timer.stamp();
				var targetTime = currentTime + $eThis.maxTimePerCase;
				var numSamples = 0;
				$exprInit;
				do {
					++numSamples;
					$expr;
				} while((currentTime = haxe.Timer.stamp()) < targetTime);
				var result = {
					name: $v{name},
					numSamples: numSamples
				};
				return result;
			}
			$eThis.addCaseFunction(f);
		}
	}

	public function run() {
		var results = [];
		for (f in cases) {
			results.push(f());
		}
		return {
			name: name,
			cases: results
		}
	}

	function addCaseFunction(f:() -> CaseResult) {
		cases.push(f);
	}
}