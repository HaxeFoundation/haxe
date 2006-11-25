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
package mtwin.mail.imap;

import mtwin.mail.Exception;
import mtwin.mail.imap.Tools;
import mtwin.mail.imap.Connection;

class Message {
	
	var mailbox : Mailbox;
	var id : Int;
	public var uid(default,null) : Int;
	var flags : Flags;
	public var structure(default,null) : BodyStructure;
	public var envelope(default,null) : Envelope;
	public var internalDate(default,null) : String;

	public static function initUid( mailbox : Mailbox, uid : Int ){
		var m = new Message();
		m.mailbox = mailbox;
		m.uid = uid;
		return m;
	}

	public function new(){
	}

	function select(){
	}

	public function usePrefetch( f : FetchResponse ){
		id = f.id;
		uid = f.uid;
		flags = f.flags;
		structure = f.structure;
		envelope = f.envelope;
		internalDate = f.internalDate;
	}

	public function getSection( ?subId : String, ?el : BodySection , ?markAsReed : Bool ){
		var cnx = mailbox.select();
		if( markAsReed == null ) markAsReed = false;
		
		var r = cnx.fetchRange( Single(uid), [if( markAsReed ) Body(SubSection(subId,el)) else BodyPeek(SubSection(subId,el))], true );
		if( r.length != 1 )
			throw ImapFetchError;
		return r.first().body;
	}

	public function markAsDeleted(){
		addFlag("\\Deleted");
	}

	public function delete(){
		markAsDeleted();
		mailbox.expunge();
	}

	public function addFlag( flag : String ){
		var cnx = mailbox.select();
		cnx.storeFlags(Single(uid), [flag], Add, true, false );
	}

	public function removeFlag( flag : String ){
		var cnx = mailbox.select();
		cnx.storeFlags(Single(uid), [flag], Remove, true, false );
	}

	public function copyTo( mb : Mailbox ){
		var cnx = mailbox.select();
		cnx.copy( Single(uid), mb.name, true );
	}

	public function hasFlag( f : String ){
		for( e in flags ){
			if( e == f ) return true;
		}
		return false;
	}


}
