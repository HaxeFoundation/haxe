package cases;

import sys.io.File;
import utest.Assert;

using StringTools;
class Ref<T> {
	public var value:T;

	public function new(value:T) {
		this.value = value;
	}

	public function toString() {
		return Std.string(value);
	}
}

@:using(cases.WeirdTreeSum.TreeTools)

enum Tree<T> {
	Node(value:Ref<T>, children:Array<Tree<T>>);
	Leaf(value:Ref<T>);
}

class TreeTools {
	static public function toString<T>(tree:Tree<T>) {
		var buf = new StringBuf();
		function loop(tabs:String, tree:Tree<T>) {
			switch (tree) {
				case Leaf(value):
					buf.add('$tabs($value)\n');
				case Node(value, children):
					buf.add('$tabs($value)\n');
					for (child in children) {
						loop(tabs + "  ", child);
					}
			}
		}
		loop("", tree);
		return buf.toString();
	}

	static public function copy<T>(tree:Tree<T>) {
		return switch (tree) {
			case Leaf(ref): Leaf(new Ref(ref.value));
			case Node(ref, children): Node(new Ref(ref.value), children.map(copy));
		}
	}
}

typedef TreeNode<T> = {
	var indent:Int;
	var children:Array<Tree<T>>;
}

class WeirdTreeSum implements utest.ITest {
	public function new() {}

	@:timeout(2000)
	public function test(async:utest.Async) {
		Thread.create(() -> {
			var fileContent = File.getContent("res/tree1.txt");
			var buf = new StringBuf();
			buf.add("(1)\n");
			for (i in 0...10) {
				buf.add(fileContent + "\n");
			}
			var tree = parseTree(buf.toString().trim())[0];
			compare(tree);
			async.done();
		});
	}

	static function compare(tree:Tree<Int>) {
		var treeSingle = tree.copy();
		var treeMulti = tree.copy();
		traverseSingleThreaded(treeSingle);
		traverseMultiThreaded(treeMulti);
		Assert.equals(treeSingle.toString(), treeMulti.toString());
	}

	static function parseTree(s:String) {
		var split = s.split("\n");
		var children = [];
		var nullNode:Dynamic = null;
		var node = {elt: {children: children, indent: -1}, next: nullNode};
		for (offset in 0...split.length) {
			var line = split[offset];
			var lineIndent = line.indexOf("(");
			var value = line.substr(lineIndent + 1, line.indexOf(")") - lineIndent - 1);
			var ref = new Ref(Std.parseInt(value));
			while (lineIndent <= node.elt.indent) {
				node = node.next;
			}
			if (offset == split.length - 1) {
				node.elt.children.push(Leaf(ref));
			} else if (lineIndent >= split[offset + 1].indexOf("(")) {
				node.elt.children.push(Leaf(ref));
			} else {
				var children = [];
				node.elt.children.push(Node(ref, children));
				node = {elt: {children: children, indent: lineIndent}, next: node};
			}
		}
		return children;
	}

	static function traverseSingleThreaded(tree:Tree<Int>) {
		return switch (tree) {
			case Leaf(value): value.value;
			case Node(value, children):
				for (child in children) {
					value.value += traverseSingleThreaded(child);
				}
				value.value;
		}
	}

	/**
		For each node, we spawn N threads where N is the number of children
		that node has. Each thread waits for a "go" message from its parent
		thread, and then calculates the node value recursively.

		Once done, it sends a "done" message to the waiter thread, which
		counts how many results we're still expecting. Once that number reaches
		0, it sends "done!" to the parent thread which may then return.

		This isn't meant to be fast or elegant, but tests message passing between
		a couple thousand threads.
	**/
	static function traverseMultiThreaded(tree:Tree<Int>) {
		return switch (tree) {
			case Leaf(value): value.value;
			case Node(value, children):
				var self = Thread.current();
				var todo = children.length;
				var valueMutex = new Mutex();
				var todoMutex = new Mutex();
				var waiterThread = Thread.create(() -> {
					while (true) {
						switch (Thread.readMessage(true)) {
							case "done":
								todoMutex.acquire();
								todo--;
								todoMutex.release();
								if (todo == 0) {
									break;
								}
							case _:
								throw "Something went wrong";
						}
					}
					self.sendMessage("done!");
				});
				var childrenThreads = [];
				for (child in children) {
					var childThread = Thread.create(() -> {
						switch (Thread.readMessage(true)) {
							case "go":
							case _: throw "Something went wrong";
						}
						valueMutex.acquire();
						value.value += traverseMultiThreaded(child);
						valueMutex.release();
						waiterThread.sendMessage("done");
					});
					childrenThreads.push(childThread);
				}
				for (childThread in childrenThreads) {
					childThread.sendMessage("go");
				}
				switch (Thread.readMessage(true)) {
					case "done!": value.value;
					case _: throw "Something went wrong";
				}
		}
	}
}
