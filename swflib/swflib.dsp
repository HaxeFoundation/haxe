# Microsoft Developer Studio Project File - Name="swflib" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) External Target" 0x0106

CFG=swflib - Win32 Bytecode
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "swflib.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "swflib.mak" CFG="swflib - Win32 Bytecode"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "swflib - Win32 Bytecode" (based on "Win32 (x86) External Target")
!MESSAGE "swflib - Win32 Native code" (based on "Win32 (x86) External Target")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""

!IF  "$(CFG)" == "swflib - Win32 Bytecode"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir ""
# PROP BASE Intermediate_Dir ""
# PROP BASE Cmd_Line "ocamake swflib.dsp"
# PROP BASE Rebuild_Opt "-all"
# PROP BASE Target_File "swflib.exe"
# PROP BASE Bsc_Name ""
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir ""
# PROP Intermediate_Dir ""
# PROP Cmd_Line "ocamake swflib.dsp"
# PROP Rebuild_Opt "-all"
# PROP Target_File "swflib.exe"
# PROP Bsc_Name ""
# PROP Target_Dir ""

!ELSEIF  "$(CFG)" == "swflib - Win32 Native code"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir ""
# PROP BASE Intermediate_Dir ""
# PROP BASE Cmd_Line "ocamake -opt swflib.dsp -o swflib_opt.exe"
# PROP BASE Rebuild_Opt "-all"
# PROP BASE Target_File "swflib_opt.exe"
# PROP BASE Bsc_Name ""
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir ""
# PROP Intermediate_Dir ""
# PROP Cmd_Line "ocamake -opt swflib.dsp -o swflib.exe"
# PROP Rebuild_Opt "-all"
# PROP Target_File "swflib.exe"
# PROP Bsc_Name ""
# PROP Target_Dir ""

!ENDIF 

# Begin Target

# Name "swflib - Win32 Bytecode"
# Name "swflib - Win32 Native code"

!IF  "$(CFG)" == "swflib - Win32 Bytecode"

!ELSEIF  "$(CFG)" == "swflib - Win32 Native code"

!ENDIF 

# Begin Source File

SOURCE=.\swf.ml
# End Source File
# End Target
# End Project
