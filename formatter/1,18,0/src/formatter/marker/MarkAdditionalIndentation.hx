package formatter.marker;

class MarkAdditionalIndentation extends MarkerBase {
	public function run() {
		parsedCode.root.filterCallback(function(token:TokenTree, index:Int):FilterResult {
			switch (token.tok) {
				case Sharp(MarkLineEnds.SHARP_IF):
					markBlockBreakingConditional(token, calcDepthDifferenceLeftCurly(token));
					markBlockBreakingConditional(token, calcDepthDifferenceRightCurly(token));
				default:
			}
			return GoDeeper;
		});
	}

	function markBlockBreakingConditional(token:TokenTree, depthDifference:Int) {
		if (token.children == null) {
			return;
		}
		var start:Null<TokenTree> = token.access().firstOf(Sharp(MarkLineEnds.SHARP_END)).token;
		if (start == null) {
			return;
		}
		// var depthDifference:Int = calcDepthDifference(token);

		if (depthDifference == 0) {
			return;
		}
		if (depthDifference < 0) {
			start = token;
		}

		var parent:Null<TokenTree> = token.parent;
		var topLevelToken:Null<TokenTree> = null;
		while ((parent != null) && (parent.tok != Root)) {
			topLevelToken = parent;
			parent = parent.parent;
		}
		if (topLevelToken == null) {
			return;
		}

		var sibling:Null<TokenTree> = topLevelToken.nextSibling;
		while (sibling != null) {
			switch (sibling.tok) {
				case BrClose:
					increaseIndentBetween(start, sibling, depthDifference);
					return;
				default:
					sibling = sibling.nextSibling;
			}
		}
		increaseIndentBetween(start, TokenTreeCheckUtils.getLastToken(topLevelToken), depthDifference);
	}

	function calcDepthDifferenceLeftCurly(token:TokenTree):Int {
		if (token.children == null) {
			return 0;
		}
		var depthIncrease:Int = 0;
		for (child in token.children) {
			var brOpens:Array<TokenTree> = child.filterCallback(function(token:TokenTree, index:Int):FilterResult {
				return switch (token.tok) {
					case Sharp(_): SkipSubtree;
					case BrOpen: FoundGoDeeper;
					default: GoDeeper;
				}
			});
			if (brOpens.length <= 0) {
				continue;
			}
			var depth:Int = 0;
			for (brOpen in brOpens) {
				if (brOpen.access().firstOf(BrClose).exists()) {
					continue;
				}
				depth++;
			}
			if (depth > depthIncrease) {
				depthIncrease = depth;
			}
		}
		return depthIncrease;
	}

	function calcDepthDifferenceRightCurly(token:TokenTree):Int {
		if (token.children == null) {
			return 0;
		}
		var depthDecrease:Int = 0;
		for (child in token.children) {
			var brClose:Array<TokenTree> = child.filterCallback(function(token:TokenTree, index:Int):FilterResult {
				return switch (token.tok) {
					case BrClose: FoundGoDeeper;
					default: GoDeeper;
				}
			});
			if (brClose.length <= 0) {
				continue;
			}
			var depth:Int = 0;
			for (brClose in brClose) {
				if (brClose.access().parent().matches(BrOpen).exists()) {
					continue;
				}
				depth++;
			}
			if (depth > depthDecrease) {
				depthDecrease = depth;
			}
		}

		return -depthDecrease;
	}
}
