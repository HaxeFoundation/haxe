# Microsoft Developer Studio Project File - Name="xml" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) External Target" 0x0106

CFG=xml - Win32 Bytecode
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "xml.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "xml.mak" CFG="xml - Win32 Bytecode"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "xml - Win32 Bytecode" (based on "Win32 (x86) External Target")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir ""
# PROP BASE Intermediate_Dir ""
# PROP BASE Cmd_Line "ocamake xml.dsp"
# PROP BASE Rebuild_Opt "-all"
# PROP BASE Target_File "xml.exe"
# PROP BASE Bsc_Name ""
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir ""
# PROP Intermediate_Dir ""
# PROP Cmd_Line "ocamake xml.dsp -P xml_parser.ml -P xml_lexer.ml -P dtd.ml -P xmlParser.ml"
# PROP Rebuild_Opt "-all"
# PROP Target_File "xml.exe"
# PROP Bsc_Name ""
# PROP Target_Dir ""
# Begin Target

# Name "xml - Win32 Bytecode"

!IF  "$(CFG)" == "xml - Win32 Bytecode"

!ENDIF 

# Begin Group "ML Files"

# PROP Default_Filter "ml;mly;mll"
# Begin Source File

SOURCE=.\dtd.ml
# End Source File
# Begin Source File

SOURCE=.\test.ml
# End Source File
# Begin Source File

SOURCE=.\xml.ml
# End Source File
# Begin Source File

SOURCE=.\xml_lexer.mll
# End Source File
# Begin Source File

SOURCE=.\xml_parser.mly
# End Source File
# Begin Source File

SOURCE=.\xmlParser.ml
# End Source File
# End Group
# Begin Group "MLI Files"

# PROP Default_Filter "mli"
# Begin Source File

SOURCE=.\dtd.mli
# End Source File
# Begin Source File

SOURCE=.\xml.mli
# End Source File
# Begin Source File

SOURCE=.\xml_lexer.mli
# End Source File
# Begin Source File

SOURCE=.\xmlParser.mli
# End Source File
# End Group
# End Target
# End Project
