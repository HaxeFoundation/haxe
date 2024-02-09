class Main {
	public static function main(){
		python.Syntax.importModule("numpy");
		python.Syntax.importAs("pandas", "pd");
		python.Syntax.importFromAs("sklearn", "metrics", "metrics");
	}
}