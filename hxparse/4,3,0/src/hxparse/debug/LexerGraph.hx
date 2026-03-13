package hxparse.debug;

#if !hxdotgraph
#error "Using this class requires -lib hxdotgraph"
#end

import hxparse.Ruleset;
import hxparse.State;
import dot.Graph;
import dot.Node;
import dot.Attribute;
using Lambda;

class LexerGraph<T> {
	
	static public function printRuleset<T>(ruleset:Ruleset<T>):String {
		var lexerGraph = new LexerGraph(ruleset);
		return lexerGraph.graph.getDotCode();
	}
	
	var graph:Graph;
	var ruleset:Ruleset<T>;
	var map:Map<State, Node>;
	
	function new(ruleset:Ruleset<T>) {
		this.ruleset = ruleset;
		this.graph = new Graph([RankDir(Lr)], true);
		map = new Map();
		processState(ruleset.state);
	}
	
	function processState(state:State) {
		if (map.exists(state)) {
			return map[state];
		}
		var attrs = [Label("")];
		if (state.finalId > -1) {
			attrs.push(Shape(Doublecircle));
		}
		
		var node = graph.node(attrs);
		map[state] = node;
		
		var targets = new Map();
		for (i in 0...256) {
			if (state.trans[i] == null) {
				continue;
			}
			var target = state.trans[i];
			if (!targets.exists(target)) {
				targets[target] = [i];
			} else {
				targets[target].push(i);
			}
		}
		
		for (target in targets.keys()) {
			var il = targets[target];
			var targetNode = processState(target);
			var edgeLabel = getRangeString(il);
			graph.edge(node, targetNode, [Label(edgeLabel)]);
		}
		
		return node;
	}
	
	function getRangeString(il:Array<Int>) {
		if (il.length > 240) {
			return "[^" + getRangeString(complementOf(il)) + "]";
		} else if (il.length == 1) {
			return printCode(il[0]);
		}
		
		var ranges = [];
		var i = 0;
		var last = -1;
		var start = -1;
		function addRange() {
			if (start == last) {
				ranges.push(printCode(start));
			} else {
				ranges.push(printCode(start) + "-" +printCode(last));
			}
		}
		while (i < il.length) {
			var cur = il[i];
			if (start == -1) {
				start = cur;
				++i;
			} else if (cur != last + 1) {
				addRange();
				start = -1;
			} else {
				++i;
			}
			last = cur;
		}
		if (start != -1) {
			addRange();
		}
		return ranges.join(" ");
	}
	
	function printCode(i:Int) {
		if (i >= 32 && i <= 0x7F) {
			return switch (i) {
				case '"'.code: '\\"';
				case '\\'.code: '\\\\';
				case ' '.code: "' '";
				case _: String.fromCharCode(i);
			}
		} else {
			return "\\\\" +i;
		}
	}
	
	function complementOf(il:Array<Int>) {
		var ret = [];
		for (i in 0...256) {
			if (!il.has(i)) {
				ret.push(i);
			}
		}
		return ret;
	}
}
