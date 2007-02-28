/*
 *  Haxe Setup
 *  Copyright (c)2006 Nicolas Cannasse
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

// this is a small program that do basic haXe setup on Windows
#include <windows.h>

static void Set( HKEY k, const char *name, DWORD t, const char *data ) {
	RegSetValueEx(k,name,0,t,(const BYTE*)data,(DWORD)strlen(data)+1);
}

int WINAPI WinMain( HINSTANCE inst, HINSTANCE prev, LPSTR lpCmdLine, int nCmdShow ) {
	char path[MAX_PATH];
	*path = '"';
	GetModuleFileName(NULL,path+1,MAX_PATH);

	// register .hxml extension
	char *s = strrchr(path,'\\') + 1;
	strcpy(s,"haxe.exe\" -prompt \"%1\"");
	HKEY k;
	RegCreateKey(HKEY_CLASSES_ROOT,".hxml\\shell\\Compile\\command",&k);
	RegSetValueEx(k,NULL,0,REG_SZ,(const BYTE*)path,(DWORD)(strlen(path)+1));
	*s = 0;

	// add %HAXEPATH% to PATH and set HAXEPATH to current path
	DWORD ktype;
	DWORD ksize = 16000;
	char *kdata = new char[16000];
	memset(kdata,0,ksize);
	RegOpenKey(HKEY_CURRENT_USER,"Environment",&k);	
	RegQueryValueEx(k,"PATH",NULL,&ktype,(LPBYTE)kdata,&ksize);
	if( strstr(kdata,"%HAXEPATH%") == NULL ) {
		char *s = kdata + strlen(kdata);
		strcpy(s,";%HAXEPATH%");
		Set(k,"PATH",REG_EXPAND_SZ,kdata);		
	}
	if( strstr(kdata,"%NEKO_INSTPATH%") == NULL ) {
		char *s = kdata + strlen(kdata);
		strcpy(s,";%NEKO_INSTPATH%");
		Set(k,"PATH",REG_EXPAND_SZ,kdata);
	}
	Set(k,"HAXEPATH",REG_SZ,path + 1);	
	s[-1] = 0;
	strcpy(strrchr(path,'\\'),"\\neko");
	Set(k,"NEKO_INSTPATH",REG_SZ,path+1);
	RegCloseKey(k);

	// inform running apps of env changes (W2K/NT systems only ?)
	DWORD unused;
	SendMessageTimeout(HWND_BROADCAST,WM_SETTINGCHANGE, 0, (LPARAM)"Environment", SMTO_ABORTIFHUNG, 5000, &unused );

	delete kdata;
	// register 
	if( strcmp(lpCmdLine,"-silent") != 0 )
		MessageBox(NULL,"Setup completed, you can start using haXe now","haxesetup",MB_OK | MB_ICONINFORMATION);
	return 0;
}
