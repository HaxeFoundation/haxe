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

package mtwin.web;

import mtwin.web.Request;

enum ActionError {
	UnknownAction(name:String);
	ObjectNotFound(id:Int);
	ActionReservedToLoggedUsers;
	ActionReservedToObjectOwner;
	ActionReservedToAdministrators;
	ActionReservedToModerators;
	CallingObjectMethodWithoutId;
}

/**
	Generic class to handle web actions.

	Add to your .htaccess :

	<FilesMatch "^(actionName|actionName|...)$">
	RewriteEngine On
	RewriteRule (.*) /index.n
	</FilesMatch>
**/
class Handler<T> {

	public static var STATIC_DEFAULT = "default";
	public static var OBJECT_DEFAULT = "objectDefault";

	var ReadWrite : Bool;
	var ReadOnly : Bool;

	var actions : Hash<Void->Void>;
	var id : Int;
	var level : Int;
	var request : mtwin.web.Request;

	public function new(){
		actions = new Hash();
		ReadWrite = true;
		ReadOnly = false;
	}

	public function execute( request:Request, ?pathLevel:Int ){
		if (pathLevel == null)
			pathLevel = 0;

		var part = request.getPathInfoPart(pathLevel);
		if (~/^[0-9]+$/.match(part)){
			id = Std.parseInt(part);
			part = request.getPathInfoPart(++pathLevel);
			if (part == "")
				part = OBJECT_DEFAULT;
		}

		this.request = request;
		this.level = pathLevel;

		if (part == "")
			part = STATIC_DEFAULT;
		if (actions.exists(part)){
			actions.get(part)();
			return;
		}
		throw UnknownAction(part);
	}

	// Methods to override when needed

	function prepareTemplate( t:String ) : Void {
		throw "not implemented";
	}

	function isLogged() : Bool {
		throw "not implemented";
		return false;
	}
	
	function isAdmin() : Bool {
		throw "not implemented";
		return false;
	}
	
	function isModerator() : Bool {
		throw "not implemented";
		return false;
	}

	function isOwner( o:T ) : Bool {
		throw "not implemented";
		return false;
	}

	function findObject( id : Int, lock:Bool ) : T {
		throw "findObject(Int) not implemented";
		return null;
	}

	// callback wrappers
	
	function object( cb:T->Void, ?lock:Bool ) : Void->Void {
		if (lock == null) lock = ReadWrite;
		var me = this;
		return function(){
			if (me.id == null)
				throw CallingObjectMethodWithoutId;
			var obj = me.findObject(me.id, lock);
			if (obj == null)
				throw ObjectNotFound(me.id);
			cb(obj);
		}
	}

	function owner( cb:T->Void ) : T->Void {
		var me = this;
		return function(u:T){
			if (!me.isOwner(u))
				throw ActionReservedToObjectOwner;
			cb(u);
		}
	}

	function handler( h:Handler<Dynamic> ) : Void->Void {
		var me = this;
		return function(){
			h.execute(me.request, me.level+1);
		}
	}

	// action declarators

	function free( n:String, ?t:String, ?cb:Void->Void ){
		var me = this;
		actions.set(n, function(){
			me.run(t,cb);
		});
	}

	function logged( n:String, ?t:String, ?cb:Void->Void ){
		var me = this;
		actions.set(n, function(){
			if (!me.isLogged())
				throw ActionReservedToLoggedUsers;
			me.run(t,cb);
		});
	}

	function admin( n:String, ?t:String, ?cb:Void->Void ){
		var me = this;
		actions.set(n, function(){
			if (!me.isAdmin())
				throw ActionReservedToAdministrators;
			me.run(t,cb);
		});
	}

	function moderator( n:String, ?t:String, ?cb:Void->Void ){
		var me = this;
		actions.set(n, function(){
			if (!me.isModerator())
				throw ActionReservedToModerators;
			me.run(t,cb);
		});
	}

	function run( ?t:String, ?cb:Void->Void ){
		if (t != null) prepareTemplate(t);
		if (cb != null) cb();
	}
}
