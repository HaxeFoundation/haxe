/*
 * Copyright (c) 2006, Motion-Twin
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY MOTION-TWIN "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE HAXE PROJECT CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 */
package mtwin.mail;

import neko.io.Socket;
import mtwin.mail.Exception;

class Smtp {

	public static function send( host : String, from : String, to : String, data : String, ?port: Int, ?user: String, ?password: String ){
		if( port == null ) port = 25;

		var cnx = new Socket();
		
		try {
			cnx.connect(Socket.resolve(host),port);
		}catch( e : Dynamic ){
			throw ConnectionError(host,port);
		}
		
		// get server init line
		var ret = StringTools.trim(cnx.input.readLine());
		if ( StringTools.endsWith(ret, "ESMTP" ) ) { //if server support extensions
			//EHLO
			cnx.write( "EHLO " + Socket.localhost() + "\r\n");
			ret = "";
			var supportLoginAuth = false;
			
			do {
				ret = StringTools.trim(cnx.input.readLine());
				if( ret.substr(0,3) != "250" ){
					cnx.close();
					throw SmtpMailFromError(ret);
				} else if ( ret.substr(4, 4) == "AUTH" && ret.indexOf("LOGIN") != -1) {
					supportLoginAuth = true;
				}
			} while(ret.substr(0,4) != "250 ");

			if ( supportLoginAuth && user != null ) { //if server support AUTH LOGIN and we have user to login with
				var base64Alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"; 
				cnx.write( "AUTH LOGIN\r\n" );
				ret = StringTools.trim(cnx.input.readLine());
				if( ret.substr(0,3) != "334" ){
					cnx.close();
					throw SmtpMailFromError(ret);
				}
				
				cnx.write( StringTools.baseEncode(user, base64Alphabet) + "\r\n" );
				ret = StringTools.trim(cnx.input.readLine());
				if( ret.substr(0,3) != "334" ){
					cnx.close();
					throw SmtpMailFromError(ret);
				}
				cnx.write( StringTools.baseEncode(password, base64Alphabet) + "\r\n" );
				ret = StringTools.trim(cnx.input.readLine());
				if( ret.substr(0,3) != "235" ){
					cnx.close();
					throw SmtpMailFromError(ret);
				}
			}
		} else {
			//HELO
			cnx.write( "HELO " + Socket.localhost() + "\r\n");
			ret = StringTools.trim(cnx.input.readLine());
			if( ret.substr(0,3) != "250" ){
				cnx.close();
				throw SmtpMailFromError(ret);
			}
		}


		cnx.write( "MAIL FROM:<" + from + ">\r\n" );
		ret = StringTools.trim(cnx.input.readLine());
		if( ret.substr(0,3) != "250" ){
			cnx.close();
			throw SmtpMailFromError(ret);
		}

		cnx.write( "RCPT TO:<" + to + ">\r\n" );
		ret = StringTools.trim(cnx.input.readLine());
		if( ret.substr(0,3) != "250" ){
			cnx.close();
			throw SmtpRcptToError(ret);
		}

		cnx.write( "DATA\r\n" );
		ret = StringTools.trim(cnx.input.readLine());
		if( ret.substr(0,3) != "354" ){
			cnx.close();
			throw SmtpDataError(ret);
		}

		if( data.substr(data.length -2,2) != "\r\n" ) 
			data += "\r\n";

		cnx.write( data + ".\r\n" );
		ret = StringTools.trim(cnx.input.readLine());
		if( ret.substr(0,3) != "250" ){
			cnx.close();
			throw SmtpSendDataError;
		}

		cnx.write( "QUIT\r\n" );
		cnx.close();
	}
	
}
