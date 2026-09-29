package unit.issues;

private typedef Content = Array<TNode>;

private typedef TNode = {
	content : Content
};

class Issue13000 extends Test {
	@:keep static function takeContent( content : Content ) {}

	@:keep static function readContent( node : TNode ) {
		return node.content;
	}

	function test() {
		var node : TNode = { content : [] };
		node.content.push({ content : [] });
		eq(1, readContent(node).length);
	}
}
