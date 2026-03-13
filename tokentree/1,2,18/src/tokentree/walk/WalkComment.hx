package tokentree.walk;

class WalkComment {
	public static function walkComment(stream:TokenStream, parent:TokenTree) {
		var progress:TokenStreamProgress = new TokenStreamProgress(stream);
		while (stream.hasMore() && progress.streamHasChanged()) {
			switch (stream.token()) {
				case Comment(_), CommentLine(_):
					var comment:TokenTree = stream.consumeToken();
					parent.addChild(comment);
				default:
					return;
			}
		}
	}

	public static function tryWalkComment(stream:TokenStream, parent:TokenTree, expect:TokenTreeDef) {
		var currentPos:Int = stream.getStreamIndex();
		var progress:TokenStreamProgress = new TokenStreamProgress(stream);
		var comments:Array<TokenTree> = [];
		while (stream.hasMore() && progress.streamHasChanged()) {
			switch (stream.token()) {
				case Comment(_), CommentLine(_):
					comments.push(stream.consumeToken());
				default:
					if (stream.matches(expect)) {
						for (comment in comments) {
							parent.addChild(comment);
						}
						return;
					}
					stream.rewindTo(currentPos);
					return;
			}
		}
	}
}