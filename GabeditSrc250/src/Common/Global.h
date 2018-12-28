/**********************************************************************************************************
Copyright (c) 2002-2017 Abdul-Rahman Allouche. All rights reserved

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated
documentation files (the Gabedit), to deal in the Software without restriction, including without limitation
the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software,
and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all copies or substantial portions
  of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
DEALINGS IN THE SOFTWARE.
************************************************************************************************************/

#ifndef __GABEDIT_GLOBAL_H__
#define __GABEDIT_GLOBAL_H__

#include <stdio.h>
#include <string.h>
#include <gdk/gdkkeysyms.h>
#include <gtk/gtk.h>
#include <glib.h>
#include <glib/gi18n.h>
#include "../Files/GabeditFileChooser.h"
#include "../Common/GabeditType.h"

#define NBNOD GABEDIT_TYPENODE_OTHER + 1

#define PROG_IS_DEMON  GABEDIT_TYPENODE_DEMON
#define PROG_IS_GAMESS  GABEDIT_TYPENODE_GAMESS
#define PROG_IS_GAUSS  GABEDIT_TYPENODE_GAUSSIAN
#define PROG_IS_MOLCAS GABEDIT_TYPENODE_MOLCAS
#define PROG_IS_MOLPRO GABEDIT_TYPENODE_MOLPRO
#define PROG_IS_MPQC  GABEDIT_TYPENODE_MPQC
#define PROG_IS_ORCA  GABEDIT_TYPENODE_ORCA
#define PROG_IS_FIREFLY  GABEDIT_TYPENODE_FIREFLY
#define PROG_IS_QCHEM  GABEDIT_TYPENODE_QCHEM
#define PROG_IS_NWCHEM  GABEDIT_TYPENODE_NWCHEM
#define PROG_IS_PSICODE  GABEDIT_TYPENODE_PSICODE
#define PROG_IS_MOPAC  GABEDIT_TYPENODE_MOPAC
#define PROG_IS_OTHER -1

#define GEOM_IS_XYZ    GABEDIT_TYPENODE_XYZ
#define GEOM_IS_ZMAT   GABEDIT_TYPENODE_GZMAT
#define GEOM_IS_OTHER -1

#define DATA_MOD_YES   1
#define DATA_MOD_NO    0

#define MAJOR_VERSION    2
#define MINOR_VERSION    5
#define MICRO_VERSION    0

/**** Structures *********/
typedef struct _FileOpen
{
 gchar *projectname; /* without .com or .log or .molden */
 gchar *datafile;
 gchar *outputfile;
 gchar *logfile;
 gchar *moldenfile;

 gchar *localhost;
 gchar *localdir;

 gchar *remotehost;
 gchar *remoteuser;
 gchar *remotepass;
 gchar *remotedir;
 gchar *command;
 GabEditNetWork netWorkProtocol;
}FileOpen;

typedef struct _FontsStyle
{
 gchar *fontname;
 GdkColor BaseColor;
 GdkColor TextColor;
}FontsStyle;

typedef struct _WidgetChildren
{
 gint nchildren;
 GtkWidget **children;
 GabeditSignalFunc *destroychildren;
}WidgetChildren;

typedef struct _User
{
 gint ndirs;
 gchar *username;
 gchar *password;
 gchar **dirs;
}User;

typedef struct _Host
{
 gint nusers;
 gchar *hostname;
 User *users;
}Host;
typedef struct _RecentHosts
{
 gint nhosts;
 Host *hosts;
}RecentHosts;

typedef struct _CommandsList
{
	gint numberOfCommands;
	gint numberOfDefaultCommand;
	gchar** commands;
}CommandsList;

typedef struct _CommandsBatch
{
	gint numberOfTypes;
	gchar** types;
	gchar** commandListAll;
	gchar** commandListUser;
	gchar** commandKill;
	gchar** jobIdTitle;
}CommandsBatch;

/**** Global variables *********/
  GtkWidget *Fenetre;
  GtkWidget *vboxlistfiles;
  GtkWidget *vboxtexts;
  GtkWidget *vboxmain;
  GtkWidget *BarreMenu;
  GtkWidget *text;
  GtkWidget *treeViewProjects;
  GtkTreeIter *noeud[NBNOD];
  GtkWidget *NoteBookText;
  GtkWidget *NoteBookInfo;
  GtkWidget *TextOutput;
  GtkWidget *TextError;
  GtkWidget *textresult;
  GtkWidget *HboxWins;
  GtkWidget *FrameWins;
  GtkWidget *FrameList;
  GtkWidget *Hpaned;
  GtkWidget *ResultEntryPass;
  GtkWidget *ResultLocalFrame;
  GtkWidget *ResultRemoteFrame;
  gint iedit;
  gint imodif;
  gint iframe;
  gchar *NameCommandGamess;
  gchar *NameCommandGaussian;
  gchar *NameCommandMolcas;
  gchar *NameCommandMolpro;
  gchar *NameCommandMPQC;
  gchar *NameCommandFireFly;
  gchar *NameCommandQChem;
  gchar *NameCommandOrca;
  gchar *NameCommandDeMon;
  gchar *NameCommandNWChem;
  gchar *NameCommandPsicode;
  gchar *NameCommandMopac;
  gchar *NameCommandPovray;
  gint ScreenWidth;
  gint ScreenHeight;
  FontsStyle FontsStyleData;
  FontsStyle FontsStyleResult;
  FontsStyle FontsStyleOther;
  FontsStyle FontsStyleLabel;

  RecentHosts recenthosts;

  int iprogram; 
  gboolean MeasureIsHide;
  FileOpen fileopen;
  gchar* lastdirectory;
  GabEditNetWork defaultNetWorkProtocol;
  gchar* pscpCommand;
  gchar* plinkCommand;
  gchar* pscpplinkDirectory;
  gchar* babelCommand;
  gchar* gamessDirectory;
  gchar* fireflyDirectory;
  gchar* orcaDirectory;
  gchar* demonDirectory;
  gchar* nwchemDirectory;
  gchar* psicodeDirectory;
  gchar* mopacDirectory;
  gchar* gaussDirectory;
  gchar* povrayDirectory;
  gchar* openbabelDirectory;
  CommandsList demonCommands;
  CommandsList gamessCommands;
  CommandsList gaussianCommands;
  CommandsList molcasCommands;
  CommandsList molproCommands;
  CommandsList mpqcCommands;
  CommandsList orcaCommands;
  CommandsList nwchemCommands;
  CommandsList psicodeCommands;
  CommandsList fireflyCommands;
  CommandsList qchemCommands;
  CommandsList mopacCommands;
  CommandsList povrayCommands;

  gchar *NameTypeBatch;
  gchar *NameCommandBatchAll;
  gchar *NameCommandBatchUser;
  gchar *NameCommandBatchKill;
  gchar *NamejobIdTitleBatch;
  CommandsBatch batchCommands;
  OpenGLOptions openGLOptions;
  gint colorMapType;
  gdouble colorMapColors[3][3];
  gdouble alpha_opacity;
  gdouble multipole_rank;

#endif /* __GABEDIT_GLOBAL_H__ */

