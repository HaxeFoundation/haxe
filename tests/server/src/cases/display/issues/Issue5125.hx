package cases.display.issues;

class Issue5125 extends DisplayTestCase {
	/**
		enum TextDocumentSyncKind {
		    Incremental;
		}

		class Main {
			public static inline var syncKind = TextDocumentSyncKind.Incremental;
			static function main() {
		        trace({-1-}syncKind);
		    }
		}
	**/
	function testStaticInlineVarType(_) {
		eq("TextDocumentSyncKind", type(1));
	}
}
