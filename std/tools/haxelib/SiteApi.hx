/*
 * Copyright (C)2005-2012 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */
package tools.haxelib;

import tools.haxelib.Data;

interface SiteApi {
	public function search( word : String ) : List<{ id : Int, name : String }>;
	public function infos( project : String ) : ProjectInfos;
	public function user( name : String ) : UserInfos;
	public function register( name : String, pass : String, mail : String, fullname : String ) : Bool;
	public function isNewUser( name : String ) : Bool;
	public function checkDeveloper( prj : String, user : String ) : Void;
	public function checkPassword( user : String, pass : String ) : Bool;
	public function getSubmitId() : String;
	
	public function processSubmit( id : String, user : String, pass : String ) : String;

	public function postInstall( project : String, version : String):Void;
}

