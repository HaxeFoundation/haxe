package unit.issues;

@:struct
private class NodeStruct {
	public var bodyID : Int;
	public var x : Int;
	public var y : Int;
}

private class NodeObj {
	public var bodyID : Int;
	public var x : Int;
	public var y : Int;
}

class Issue12118 extends Test {

	#if hl
	function testBlitStruct() {
		var nodes = initNodeStruct(5, 10);
		nodes.blit(NodeStruct, 1, nodes, 2, 2);
		eq(0, nodes[0].bodyID);
		eq(2, nodes[1].bodyID);
		eq(3, nodes[2].bodyID);
		eq(3, nodes[3].bodyID);
		eq(4, nodes[4].bodyID);
	}

	function testBlitObj() {
		var nodes = initNodeObj(5, 10);
		nodes.blit(NodeObj, 1, nodes, 2, 2);
		eq(0, nodes[0].bodyID);
		eq(2, nodes[1].bodyID);
		eq(3, nodes[2].bodyID);
		eq(3, nodes[3].bodyID);
		eq(4, nodes[4].bodyID);
	}

	function testSetStruct() {
		var nodes = initNodeStruct(5, 10);
		nodes.unsafeSet(1, nodes[3]);
		eq(3, nodes[1].bodyID);
		eq(13, nodes[1].x);
		eq(0, nodes[1].y);
		eq(nodes[3].bodyID, nodes[1].bodyID);
		eq(nodes[3].x, nodes[1].x);
		eq(nodes[3].y, nodes[1].y);
	}

	function testSetObj() {
		var nodes = initNodeObj(5, 10);
		nodes.unsafeSet(1, nodes[3]);
		eq(3, nodes[1].bodyID);
		eq(13, nodes[1].x);
		eq(0, nodes[1].y);
		eq(nodes[3].bodyID, nodes[1].bodyID);
		eq(nodes[3].x, nodes[1].x);
		eq(nodes[3].y, nodes[1].y);
	}

	private function initNodeStruct( count : Int, offset : Int ) {
		var nodes = hl.CArray.alloc(NodeStruct, count);
		for( i in 0...count ) {
			var node = nodes[i];
			node.bodyID = i;
			node.x = offset + i;
			node.y = 0;
		}
		return nodes;
	}

	private function initNodeObj( count : Int, offset : Int ) {
		var nodes = hl.CArray.alloc(NodeObj, count);
		for( i in 0...count ) {
			var node = nodes[i];
			node.bodyID = i;
			node.x = offset + i;
			node.y = 0;
		}
		return nodes;
	}
	#end
}
