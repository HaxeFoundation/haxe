package mtwin.mail;

import neko.Socket;
import mtwin.mail.Exception;

class Smtp {

	public static function send( host : String, from : String, to : String, data : String, ?port: Int ){
		if( port == null ) port = 25;

		var cnx = new Socket();
		
		try {
			cnx.connect(Socket.resolve(host),port);
		}catch( e : Dynamic ){
			throw ConnectionError(host,port);
		}
		
		// get server init line
		cnx.readLine();

		cnx.write( "MAIL FROM:<" + from + ">\r\n" );
		var ret = StringTools.trim(cnx.readLine());
		if( ret.substr(0,3) != "250" ){
			cnx.close();
			throw SmtpMailFromError(ret);
		}

		cnx.write( "RCPT TO:<" + to + ">\r\n" );
		ret = StringTools.trim(cnx.readLine());
		if( ret.substr(0,3) != "250" ){
			cnx.close();
			throw SmtpRcptToError(ret);
		}

		cnx.write( "DATA\r\n" );
		ret = StringTools.trim(cnx.readLine());
		if( ret.substr(0,3) != "354" ){
			cnx.close();
			throw SmtpDataError(ret);
		}

		if( data.substr(data.length -2,2) != "\r\n" ) 
			data += "\r\n";

		cnx.write( data + ".\r\n" );
		ret = StringTools.trim(cnx.readLine());
		if( ret.substr(0,3) != "250" ){
			cnx.close();
			throw SmtpSendDataError;
		}

		cnx.write( "QUIT\r\n" );
		cnx.close();
	}
	
}
