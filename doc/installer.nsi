; Haxe/Neko Install script

;--------------------------------

!include "MUI.nsh"
!include "LogicLib.nsh"
!include "WordFunc.nsh"
!include "winmessages.nsh"
!include "EnvVarUpdate.nsh"

;--------------------------------

; Define version info
!define VERSION "3.0.0"

; Define Neko info
!define NEKO_VERSION "2.0.0"

; Installer details
VIAddVersionKey "CompanyName" "Haxe Foundation"
VIAddVersionKey "ProductName" "Haxe Installer"
VIAddVersionKey "LegalCopyright" "Haxe Foundation 2005-2013"
VIAddVersionKey "FileDescription" "Haxe Installer"
VIAddVersionKey "ProductVersion" "${VERSION}.0"
VIAddVersionKey "FileVersion" "${VERSION}.0"
VIProductVersion "${VERSION}.0"

; The name of the installer
Name "Haxe ${VERSION}"

; The captions of the installer
Caption "Haxe ${VERSION} Setup"
UninstallCaption "Haxe ${VERSION} Uninstall"

; The file to write
OutFile "haxe-${VERSION}-win.exe"

; Default installation folder
InstallDir "$PROGRAMFILES\HaxeFoundation\"

; Define executable files
!define EXECUTABLE "$INSTDIR\haxe\haxe.exe"
!define HaxeLIB "$INSTDIR\Haxe\haxelib.exe"
!define NEKOEXE "$INSTDIR\neko\neko.exe"

; Vista redirects $SMPROGRAMS to all users without this
RequestExecutionLevel admin

; Use replace and version compare
!insertmacro WordReplace
!insertmacro VersionCompare

; Required props
SetFont /LANG=${LANG_ENGLISH} "Tahoma" 8
SetCompressor /SOLID lzma
CRCCheck on
XPStyle on

;--------------------------------

; Interface Configuration

!define MUI_HEADERIMAGE
!define MUI_ABORTWARNING
!define MUI_HEADERIMAGE_BITMAP "images\Banner.bmp"
!define MUI_WELCOMEFINISHPAGE_BITMAP "images\Wizard.bmp"
!define MUI_UNWELCOMEFINISHPAGE_BITMAP "images\Wizard.bmp"
!define MUI_PAGE_HEADER_SUBTEXT "Please view the license before installing Haxe ${VERSION}."
!define MUI_WELCOMEPAGE_TEXT "This wizard will guide you through the installation of $(^NameDA).\r\n\r\nIt is recommended that you close all other applications before starting Setup. This will make it possible to update relevant system files without having to reboot your computer.\r\n\r\n$_CLICK"

;--------------------------------

; Pages

!insertmacro MUI_PAGE_WELCOME
!insertmacro MUI_PAGE_COMPONENTS
!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_INSTFILES
!insertmacro MUI_PAGE_FINISH
!insertmacro MUI_UNPAGE_WELCOME
!insertmacro MUI_UNPAGE_CONFIRM
!insertmacro MUI_UNPAGE_COMPONENTS
!insertmacro MUI_UNPAGE_INSTFILES
!insertmacro MUI_UNPAGE_FINISH
!insertmacro MUI_LANGUAGE "English"

;--------------------------------

; InstallTypes

InstType "Default"
InstType "un.Default"
InstType "un.Full"

;--------------------------------

; Functions



Function .onInit



FunctionEnd

;--------------------------------

; Install Sections

!define env_hklm 'HKLM "SYSTEM\CurrentControlSet\Control\Session Manager\Environment"'
!define env_hkcu 'HKCU "Environment"'

Section "Haxe ${VERSION}" Main

	SectionIn 1 2 RO
	SetOverwrite on

	SetOutPath "$INSTDIR\haxe"

	File /r /x .svn /x *.db /x Exceptions.log /x .local /x .multi /x *.pdb /x *.vshost.exe /x *.vshost.exe.config /x *.vshost.exe.manifest "resources\haxe\*.*"

	ExecWait "$INSTDIR\haxe\haxesetup.exe -silent"

	WriteUninstaller "$INSTDIR\Uninstall.exe"

SectionEnd

Section "Neko ${NEKO_VERSION}" Neko

	SectionIn 1 2
	SetOverwrite on

	SetOutPath "$INSTDIR\neko"

	File /r /x .svn /x *.db /x Exceptions.log /x .local /x .multi /x *.pdb /x *.vshost.exe /x *.vshost.exe.config /x *.vshost.exe.manifest "resources\neko\*.*"

SectionEnd




;--------------------------------

; Install section strings

!insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
!insertmacro MUI_DESCRIPTION_TEXT ${Main} "Installs Haxe and other core files."
!insertmacro MUI_DESCRIPTION_TEXT ${Neko} "Installs Neko, which is required by various Haxe tools."
!insertmacro MUI_FUNCTION_DESCRIPTION_END

;--------------------------------

; Uninstall Sections

Section "un.Haxe" UninstMain

	RMDir /r "$INSTDIR\haxe"
	${un.EnvVarUpdate} $0 "PATH" "R" "HKCU" "%HAXEPATH%"
	DeleteRegValue ${env_hkcu} HAXEPATH
	SendMessage ${HWND_BROADCAST} ${WM_WININICHANGE} 0 "STR:Environment" /TIMEOUT=5000

SectionEnd

Section "un.Neko" UninstNeko

	RMDir /r "$INSTDIR\neko"
	${un.EnvVarUpdate} $0 "PATH" "R" "HKCU" "%NEKO_INSTPATH%"
	DeleteRegValue ${env_hkcu} NEKO_INSTPATH
	SendMessage ${HWND_BROADCAST} ${WM_WININICHANGE} 0 "STR:Environment" /TIMEOUT=5000

SectionEnd

;--------------------------------

; Uninstall section strings

!insertmacro MUI_UNFUNCTION_DESCRIPTION_BEGIN
!insertmacro MUI_DESCRIPTION_TEXT ${UninstMain} "Uninstalls Haxe and other core files."
!insertmacro MUI_DESCRIPTION_TEXT ${UninstNeko} "Uninstalls Neko."
!insertmacro MUI_UNFUNCTION_DESCRIPTION_END

;--------------------------------
