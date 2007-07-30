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
/*
signature ImapMailbox = {
	name: String,
	flags: ImapFlags,
	hasChildren: Bool
}
*/
package mtwin.mail.imap;

import mtwin.mail.imap.Tools;
import mtwin.mail.Exception;

class Mailbox {
	static var PREFETCH_SECTION = [Section.Uid,Section.InternalDate,Section.Envelope,Section.BodyStructure,Section.Flags];

	var cnx : Connection;
	public var name(default,null) : String;
	var flags : Flags;
	public var children : Array<Mailbox>;
	public var parent(default,null) : Mailbox;

	public var length(default,null) : Int;
	public var firstUnseen(default,null) : Int;
	public var recent(default,null) : Int;
	public var unseen(default,null) : Int;

	public static function init( c : Connection, n : String, f : Flags ){
		return new Mailbox( c,n,f );
	}

	function new( c, n, f ){
		cnx = c;
		name = n;
		flags = f;
		children = new Array();
	}

	public function select(){
		if( hasFlag("\\Noselect") ) throw NoSelect;

		var r = cnx.select( name );
		if( r != null ){
			length = r.exists;
			firstUnseen = r.firstUnseen;
			recent = r.recent;
		}
		return cnx;
	}

	public function hasFlag( f : String ){
		for( e in flags ){
			if( e == f ) return true;
		}
		return false;
	}

	public function list( ?start : Int, ?end : Int, ?fPrefetch : Bool ){
		select();

		if( start == null ) start = 1;
		if( end == null ) end = length;
		if( fPrefetch == null ) fPrefetch = true;
		
		start = Std.int(Math.max(1,Math.min(length,start)));
		end = Std.int(Math.max(1,Math.min(length,end)));
		
		var r = cnx.fetchRange( Range(start,end), if( fPrefetch ) PREFETCH_SECTION else [Uid] );
		var ret = new List();
		for( m in r ){
			var t = Message.initUid(this,m.uid);
			t.usePrefetch(m);
			ret.add( t );
		}
		return ret;
	}

	public function syncStatus(){
		if( hasFlag("\\Noselect") ) throw NoSelect;

		var r = cnx.status( name );
		length = r.get("MESSAGES");
		unseen = r.get("UNSEEN");
		recent = r.get("RECENT");
	}

	public function get( uid : Int, ?fPrefetch : Bool ){
		select();

		if( fPrefetch == null ) fPrefetch = true;
		
		var ret = Message.initUid( this, uid );
		if( fPrefetch ) prefetchOne( ret );
		return ret;
	}

	public function prefetchOne( m : Message ){
		var l = new List();
		l.add( m );
		prefetch(l);
	}

	public function prefetch( l : List<Message> ){
		if( l.length == 0 )
			return;
		select();

		var a = new Array();
		var h = new IntHash();
		for( m in l ){
			a.push( Single(m.uid) );
			h.set(m.uid,m);
		}
		var r = cnx.fetchRange( Composite(a), PREFETCH_SECTION, true );
		for( e in r ){
			h.get(e.uid).usePrefetch( e );
		}
	}

	public function search( pattern : String, ?fPrefetch : Bool ){
		select();

		if( fPrefetch == null ) fPrefetch = true;
		var r = cnx.search( pattern, true );
		var ret = new List();
		for( uid in r ){
			ret.add( Message.initUid(this,uid) );
		}
		if( fPrefetch ) prefetch( ret );
		return ret;
	}

	public function sort( criteria : String, ?pattern : String, ?charset : String, ?fPrefetch : Bool ){
		select();

		if( pattern == null ) pattern = "ALL";
		if( charset == null ) charset = "US-ASCII";
		if( fPrefetch == null ) fPrefetch = true;

		var r = cnx.sort( criteria, pattern, charset, true );
		var ret = new List();
		for( uid in r ){
			ret.add( Message.initUid(this,uid) );
		}
		if( fPrefetch ) prefetch( ret );
		return ret;
	}

	public function expunge(){
		select();
		cnx.expunge();
	}

	public function add( content : String, ?flags : Flags ){
		cnx.append( name, content, flags );
	}
}
