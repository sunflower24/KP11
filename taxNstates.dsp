# Microsoft Developer Studio Project File - Name="taxNstates" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) QuickWin Application" 0x0107

CFG=taxNstates - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "taxNstates.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "taxNstates.mak" CFG="taxNstates - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "taxNstates - Win32 Release" (based on "Win32 (x86) QuickWin Application")
!MESSAGE "taxNstates - Win32 Debug" (based on "Win32 (x86) QuickWin Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "taxNstates - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
# ADD BASE F90 /compile_only /include:"Release/" /libs:qwin /nologo /warn:nofileopt
# ADD F90 /compile_only /include:"Release/" /libs:qwin /nologo /warn:nofileopt
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /entry:"WinMainCRTStartup" /subsystem:windows /machine:I386 /nodefaultlib:"dfconsol.lib"
# ADD LINK32 kernel32.lib /nologo /entry:"WinMainCRTStartup" /subsystem:windows /machine:I386 /nodefaultlib:"dfconsol.lib"

!ELSEIF  "$(CFG)" == "taxNstates - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "taxNstates___Win32_Debug"
# PROP BASE Intermediate_Dir "taxNstates___Win32_Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "taxNstates___Win32_Debug"
# PROP Intermediate_Dir "taxNstates___Win32_Debug"
# PROP Target_Dir ""
# ADD BASE F90 /check:bounds /compile_only /debug:full /include:"taxNstates___Win32_Debug/" /libs:qwin /nologo /warn:argument_checking /warn:nofileopt
# ADD F90 /check:bounds /compile_only /debug:full /include:"taxNstates___Win32_Debug/" /libs:qwin /nologo /warn:argument_checking /warn:nofileopt
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /GZ /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /entry:"WinMainCRTStartup" /subsystem:windows /debug /machine:I386 /nodefaultlib:"dfconsol.lib" /pdbtype:sept
# ADD LINK32 kernel32.lib /nologo /entry:"WinMainCRTStartup" /subsystem:windows /debug /machine:I386 /nodefaultlib:"dfconsol.lib" /pdbtype:sept

!ENDIF 

# Begin Target

# Name "taxNstates - Win32 Release"
# Name "taxNstates - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat;f90;for;f;fpp"
# Begin Source File

SOURCE=.\autval.f90
DEP_F90_AUTVA=\
	".\taxNstates___Win32_Debug\params.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\autvalsav.f90
DEP_F90_AUTVAL=\
	".\taxNstates___Win32_Debug\params.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\cmarkets.f90
DEP_F90_CMARK=\
	".\taxNstates___Win32_Debug\params.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\condist.f90
DEP_F90_CONDI=\
	".\taxNstates___Win32_Debug\params.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\condistexinc.f90
DEP_F90_CONDIS=\
	".\taxNstates___Win32_Debug\params.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\dgrid.f90
DEP_F90_DGRID=\
	".\taxNstates___Win32_Debug\params.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\dgrid2.f90
DEP_F90_DGRID2=\
	".\taxNstates___Win32_Debug\params.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\diss.f90
DEP_F90_DISS_=\
	".\taxNstates___Win32_Debug\params.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\dissexinc.f90
DEP_F90_DISSE=\
	".\taxNstates___Win32_Debug\params.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\enforce.f90
DEP_F90_ENFOR=\
	".\taxNstates___Win32_Debug\params.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\enforceexinc.f90
DEP_F90_ENFORC=\
	".\taxNstates___Win32_Debug\params.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\exincmarkets.f90
DEP_F90_EXINC=\
	".\taxNstates___Win32_Debug\params.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\Fputil.f
# End Source File
# Begin Source File

SOURCE=.\gini.f90
DEP_F90_GINI_=\
	".\taxNstates___Win32_Debug\params.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\guess.f90
DEP_F90_GUESS=\
	".\taxNstates___Win32_Debug\params.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\iniguess.f90
DEP_F90_INIGU=\
	".\taxNstates___Win32_Debug\params.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\loadfun.f90
DEP_F90_LOADF=\
	".\taxNstates___Win32_Debug\params.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\main.f90
DEP_F90_MAIN_=\
	".\taxNstates___Win32_Debug\params.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\Markov.f
DEP_F90_MARKO=\
	".\taxNstates___Win32_Debug\params.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\mchain2.f90
DEP_F90_MCHAI=\
	".\taxNstates___Win32_Debug\params.mod"\
	{$(INCLUDE)}"MSIMSL.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\Newton.f90
DEP_F90_NEWTO=\
	".\taxNstates___Win32_Debug\params.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\params.f90
# End Source File
# Begin Source File

SOURCE=.\resid.f90
DEP_F90_RESID=\
	".\taxNstates___Win32_Debug\params.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\residexinc.f90
DEP_F90_RESIDE=\
	".\taxNstates___Win32_Debug\params.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\resource.f90
DEP_F90_RESOU=\
	".\taxNstates___Win32_Debug\params.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\resourceexinc.f90
DEP_F90_RESOUR=\
	".\taxNstates___Win32_Debug\params.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\sty.f90
DEP_F90_STY_F=\
	".\taxNstates___Win32_Debug\params.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\sys1.f90
DEP_F90_SYS1_=\
	".\taxNstates___Win32_Debug\params.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\sys2.f90
DEP_F90_SYS2_=\
	".\taxNstates___Win32_Debug\params.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\sys3.f90
DEP_F90_SYS3_=\
	".\taxNstates___Win32_Debug\params.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\sysbin.f90
DEP_F90_SYSBI=\
	".\taxNstates___Win32_Debug\params.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\sysex.f90
DEP_F90_SYSEX=\
	".\taxNstates___Win32_Debug\params.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\syssav.f90
DEP_F90_SYSSA=\
	".\taxNstates___Win32_Debug\params.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\values.f90
DEP_F90_VALUE=\
	".\taxNstates___Win32_Debug\params.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\xdemand.f90
DEP_F90_XDEMA=\
	".\taxNstates___Win32_Debug\params.mod"\
	
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl;fi;fd"
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# End Group
# End Target
# End Project
