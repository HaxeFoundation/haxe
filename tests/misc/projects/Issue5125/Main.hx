enum TextDocumentSyncKind {
    Incremental;
}

class Main {
	public static inline var syncKind = TextDocumentSyncKind.Incremental;
	static function main() {
        trace(syncKind);
    }
}