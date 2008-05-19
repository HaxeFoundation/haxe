package unit;

class RemotingServer {

	static function main() {
		var ctx = RemotingApi.context();
		if( !haxe.remoting.HttpConnection.handleRequest(ctx) )
			throw "Invalid request";
	}

}