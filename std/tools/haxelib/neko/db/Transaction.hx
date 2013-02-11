/*
 * Copyright (c) 2005, The haXe Project Contributors
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
 * THIS SOFTWARE IS PROVIDED BY THE HAXE PROJECT CONTRIBUTORS "AS IS" AND ANY
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
package neko.db;

class Transaction {

	public static function isDeadlock(e : Dynamic) {
		return Std.is(e,String) && (~/Deadlock found/.match(e) || ~/Lock wait timeout/.match(e));
	}

	private static function runMainLoop(mainFun,logError,count) {
		try {
			mainFun();
		} catch( e : Dynamic ) {
			if( count > 0 && isDeadlock(e) ) {
				Manager.cleanup();
				Manager.cnx.rollback(); // should be already done, but in case...
				Manager.cnx.startTransaction();
				runMainLoop(mainFun,logError,count-1);
				return;
			}
			if( logError == null ) {
				Manager.cnx.rollback();
				neko.Lib.rethrow(e);
			}
			logError(e); // should ROLLBACK if needed
		}
	}

	public static function main( cnx, mainFun : Void -> Void, logError : Dynamic -> Void ) {
		Manager.initialize();
		Manager.cnx = cnx;
		Manager.cnx.startTransaction();
		runMainLoop(mainFun,logError,3);
		try {
			Manager.cnx.commit();
		} catch( e : String ) {
			// sqlite can have errors on commit
			if( ~/Database is busy/.match(e) )
				logError(e);
		}
		Manager.cnx.close();
		Manager.cnx = null;
		Manager.cleanup();
	}

}