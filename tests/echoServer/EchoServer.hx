class EchoServer {
	static function main() {
		Sys.print(neko.Web.getPostData());
		Sys.sleep(0.3);
	}
}