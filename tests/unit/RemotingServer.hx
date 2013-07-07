package unit;

class RemotingServer extends neko.net.ThreadRemotingServer {

	static var HOST = "dev.unit-tests";
	static var PORT = 1999;

	override function initClientApi( cnx : haxe.remoting.SocketConnection, ctx : haxe.remoting.Context ) {
		RemotingApi.context(ctx);
		cnx.setErrorLogger(function(path,args,e) {
			// ignore invalid calls or exceptions in methods
		});
	}

	override function onXml( cnx : haxe.remoting.SocketConnection, data : String ) {
		if( data == "<policy-file-request/>" ) {
			var str = "<cross-domain-policy>";
			str += '<allow-access-from domain="'+HOST+'" to-ports="'+PORT+'"/>';
			str += "</cross-domain-policy>";
			str += "\x00";
			cnx.getProtocol().socket.write(str);
			return;
		}
		super.onXml(cnx,data);
	}

	static function main() {
		if( neko.Web.isModNeko ) {
			var ctx = RemotingApi.context();
			if( !haxe.remoting.HttpConnection.handleRequest(ctx) )
				throw "Invalid request";
			return;
		}
		var s = new RemotingServer();
		trace("Starting server on "+HOST+":"+PORT);
		s.run(HOST,PORT);
	}

}