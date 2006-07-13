package mtwin.mail;

import neko.Socket;

enum SmtpException extends mtwin.mail.Exception {
	ConnectionError(host:String,port:Int);
	MailFromError(e:String);
	RcptToError(e:String);
	DataError(e:String);
	SendDataError;
}

class Smtp {

	public static function send( host : String, from : String, to : String, data : String ){
		var cnx = new Socket();
		
		try {
			cnx.connect(Socket.resolve(host),25);
		}catch( e : Dynamic ){
			throw ConnectionError(hort,25);
		}
		
		// get server init line
		cnx.readLine();

		cnx.write( "MAIL FROM:<" + from + ">\r\n" );
		var ret = StringTools.trim(cnx.readLine());
		if( ret.substr(0,3) != "250" ){
			cnx.close();
			throw MailFromError(ret);
		}

		cnx.write( "RCPT TO:<" + to + ">\r\n" );
		ret = StringTools.trim(cnx.readLine());
		if( ret.substr(0,3) != "250" ){
			cnx.close();
			throw RcptToError(ret);
		}

		cnx.write( "DATA\r\n" );
		ret = StringTools.trim(cnx.readLine());
		if( ret.substr(0,3) != "354" ){
			cnx.close();
			throw DataError(ret);
		}

		if( data.substr(data.length -2,2) != "\r\n" ) 
			data += "\r\n";

		cnx.write( data + ".\r\n" );
		ret = StringTools.trim(cnx.readLine());
		if( ret.substr(0,3) != "250" ){
			cnx.close();
			throw SendDataError;
		}

		cnx.write( "QUIT\r\n" );
		cnx.close();
	}
	
}
