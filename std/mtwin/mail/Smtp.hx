package mtwin.mail;

import neko.Socket;

class SmtpException extends mtwin.mail.Exception {
}

class Smtp {

	public static function send( host : String, from : String, to : String, data : String ){
		var cnx = new Socket();
		
		try {
			cnx.connect(Socket.resolve(host),25);
		}catch( e : Dynamic ){
			throw new SmtpException("SMTP connection failed: "+e);
		}
		
		// get server init line
		cnx.readLine();

		cnx.write( "MAIL FROM:<" + from + ">\r\n" );
		var ret = StringTools.trim(cnx.readLine());
		if( ret.substr(0,3) != "250" ){
			cnx.close();
			throw new SmtpException("SMTP error on FROM : " + ret);
		}

		cnx.write( "RCPT TO:<" + to + ">\r\n" );
		ret = StringTools.trim(cnx.readLine());
		if( ret.substr(0,3) != "250" ){
			cnx.close();
			throw new SmtpException("SMTP error on RCPT : " + ret);
		}

		cnx.write( "DATA\r\n" );
		ret = StringTools.trim(cnx.readLine());
		if( ret.substr(0,3) != "354" ){
			cnx.close();
			throw new SmtpException("SMTP error on DATA : " + ret);
		}

		if( data.substr(data.length -2,2) != "\r\n" ) 
			data += "\r\n";

		cnx.write( data + ".\r\n" );
		ret = StringTools.trim(cnx.readLine());
		if( ret.substr(0,3) != "250" ){
			cnx.close();
			throw new SmtpException("SMTP error on mail content: " + ret);
		}

		cnx.write( "QUIT\r\n" );
		cnx.close();
	}
	
}
