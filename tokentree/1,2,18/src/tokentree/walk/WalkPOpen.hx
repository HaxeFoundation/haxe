package tokentree.walk;

import tokentree.walk.WalkSharp.WalkSharpConsts;

class WalkPOpen {
	public static function walkPOpen(stream:TokenStream, parent:TokenTree, walkTrailingComments:Bool = true):TokenTree {
		var pOpen:TokenTree = stream.consumeTokenDef(POpen);
		parent.addChild(pOpen);
		stream.applyTempStore(pOpen);
		WalkPOpen.walkPOpenParts(stream, pOpen);
		pOpen.addChild(stream.consumeTokenDef(PClose));
		if (walkTrailingComments) {
			WalkComment.walkComment(stream, parent);
		}
		if (stream.hasMore()) {
			switch (stream.token()) {
				case Arrow:
					var arrow:TokenTree = stream.consumeToken();
					pOpen.addChild(arrow);
					WalkBlock.walkBlock(stream, arrow);
				case Sharp(WalkSharpConsts.IF):
					switch (parent.tok) {
						case Sharp(WalkSharpConsts.IF):
						default: WalkSharp.walkSharp(stream, parent, WalkStatement.walkStatement);
					}
				default:
			}
		}
		return pOpen;
	}

	public static function walkPOpenParts(stream:TokenStream, parent:TokenTree) {
		var progress:TokenStreamProgress = new TokenStreamProgress(stream);
		while (progress.streamHasChanged()) {
			switch (stream.token()) {
				case BrOpen:
					WalkBlock.walkBlock(stream, parent);
				case BkOpen:
					WalkArrayAccess.walkArrayAccess(stream, parent);
				case PClose:
					break;
				case Sharp(_):
					WalkSharp.walkSharp(stream, parent, WalkPOpen.walkPOpenParts);
				case Comma:
					var comma:TokenTree = stream.consumeToken();
					var child:Null<TokenTree> = parent.getLastChild();
					if (child == null) child = parent;
					child.addChild(comma);
				default:
					WalkStatement.walkStatement(stream, parent);
			}
		}
	}
}