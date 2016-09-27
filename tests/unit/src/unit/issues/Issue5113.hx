package unit.issues;

private class Node {

    var parentNode:Node;

    public function new() { }

    public function getParent():Node {
        return parentNode;
    }

    public function addChild(node:Node) {
        node.parentNode = this;
    }
}


private class Element extends Node {

    public var children:Array<Node> = [];

    public function new() {
        super();
    }

    override public function getParent():Element {
        return cast parentNode;
    }
}

class Issue5113 extends unit.Test {
	function test() {
        var childEl = new Element();
        var parentEl = new Element();
        var parentNode = new Node();

        parentEl.addChild(childEl);
        eq(0, childEl.getParent().children.length);
	}
}