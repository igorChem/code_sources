/* Gamess.c */
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


#include "../../Config.h"
#include "../Common/Global.h"
#include "../Gamess/GamessTypes.h"
#include "../Gamess/GamessGlobal.h"
#include "../Gamess/GamessMolecule.h"
#include "../Gamess/GamessControl.h"
#include "../Gamess/GamessBasis.h"
#include "../Gamess/GamessGuess.h"
#include "../Gamess/GamessSCF.h"
#include "../Utils/UtilsInterface.h"
#include "../Geometry/GeomGlobal.h"
#include "../Geometry/InterfaceGeom.h"
#include "../Utils/Utils.h"
#include "../Utils/AtomsProp.h"
#include "../Utils/Constants.h"
#include "../Utils/GabeditTextEdit.h"

static GtkWidget *Wins=NULL;
static gboolean newFile = FALSE;
/************************************************************************************************************/
static void setColors()
{
	static gboolean first = TRUE;

	if(!first) return;
	first = FALSE;

	gamessColorFore.keyWord.red = 65535;
	gamessColorFore.keyWord.green = 0;
	gamessColorFore.keyWord.blue = 0;

	gamessColorBack.keyWord.red = (gushort)(65535/1.2);
	gamessColorBack.keyWord.green = (gushort)(65535/1.2);
	gamessColorBack.keyWord.blue = (gushort)(65535/1.2);

	gamessColorFore.description.red = 0;
	gamessColorFore.description.green = 65535;
	gamessColorFore.description.blue = 65535;

	gamessColorBack.description.red = (gushort)(65535/2.0);
	gamessColorBack.description.green = (gushort)(65535/2.0);
	gamessColorBack.description.blue = (gushort)(65535/2.0);

}
/************************************************************************************************************/
void destroyWinsGamess(GtkWidget *win)
{
	destroy(Wins,NULL);
	Wins = NULL;
	initGamessMoleculeButtons();
	/*
	freeGamessFunctionals();
	freeGamessStdFunctionals();
	freeGamessMole();
	freeGamessGuessWaveFunction();
	freeGamessMolecule();
	freeGamessMpqc();
	freeGamessBasis();
	freeGamessOptimization();
	*/
}
/************************************************************************************************************/
static void toCancelWin(GtkWidget* win,gpointer data)
{
	Cancel_YesNo(win, data, destroy_children);
}
/************************************************************************************************************/
static void putTitleInTextEditor()
{
        gchar buffer[BSIZE];

	sprintf(buffer,"%c ================================================================\n",'!');
        gabedit_text_insert (GABEDIT_TEXT(text), NULL, NULL, NULL, buffer,-1);

	sprintf(buffer,"%c This file was generated by Gabedit\n",'!');
        gabedit_text_insert (GABEDIT_TEXT(text), NULL, NULL, NULL, buffer,-1);

	sprintf(buffer,"%c ================================================================\n",'!');
        gabedit_text_insert (GABEDIT_TEXT(text), NULL, NULL, NULL, buffer,-1);
}
/************************************************************************************************************/
static void putInfoInTextEditor(GtkWidget *button, gpointer data)
{
	setColors();
	if(newFile)
	{
		reset_name_files();
		ClearText(text);
	}
	else
	{
        	gint nchar;
        	nchar=gabedit_text_get_length(GABEDIT_TEXT(text));
        	gabedit_text_set_point(GABEDIT_TEXT(text),nchar);
	}
	data_modify(TRUE);

	putTitleInTextEditor();
	putGamessControlInfoInTextEditor();
	putGamessChargeAndSpinInfoInTextEditor();

	if(!gamessSemiEmperical()) putGamessBasisInfoInTextEditor();
	putGamessGuessInfoInTextEditor();
	putGamessSCFInfoInTextEditor();
	putGamessGeometryInfoInTextEditor();

	iprogram = PROG_IS_GAMESS;
	fileopen.command=g_strdup(NameCommandGamess);
	gtk_notebook_set_current_page((GtkNotebook*)NoteBookText,0);
}
/*********************************************************************************************/
static GtkWidget* addHboxToTable(GtkWidget* table, gint i, gint j, gint ki, gint kj)
{
	GtkWidget *hbox = gtk_hbox_new(TRUE, 5);

	gtk_table_attach(GTK_TABLE(table),hbox,j,j+kj,i,i+ki,
		(GtkAttachOptions)	(GTK_FILL | GTK_EXPAND),
		(GtkAttachOptions)	(GTK_FILL | GTK_EXPAND),
                  3,3);

	return hbox;
}
/************************************************************************************************************/
static void gamessInputFileWindow(gboolean newInputFile)
{
	GtkWidget *button;
	GtkWidget *hbox = NULL;
	GtkWidget *hboxSymmetryChargeMultiplicity = NULL;

	GtkWidget *table = gtk_table_new(6,2,FALSE);

	newFile = newInputFile;

	initGamessMolecule();
	setGamessMolecule();

	if(gamessMolecule.numberOfAtoms <1)
	{
		Message(
			_(
			"You must initially define your geometry.\n\n"
			"From the principal Menu select : Geometry/Draw\n"
			"and draw (or read) your molecule."),
			_("Error"),TRUE);
		return;
	}

	if(Wins) destroy_children(Wins);

	Wins= gtk_dialog_new ();
	gtk_window_set_position(GTK_WINDOW(Wins),GTK_WIN_POS_NONE);
	gtk_window_set_transient_for(GTK_WINDOW(Wins),GTK_WINDOW(Fenetre));
	gtk_window_set_title(&GTK_DIALOG(Wins)->window,_("Gamess input"));
    	gtk_window_set_modal (GTK_WINDOW (Wins), TRUE);

	init_child(Wins, destroyWinsGamess,_(" Gamess input "));
	g_signal_connect(G_OBJECT(Wins),"delete_event",(GCallback)destroy_children,NULL);

	gtk_widget_realize(Wins);

	button = create_button(Wins,_("Cancel"));
	gtk_box_pack_start (GTK_BOX( GTK_DIALOG(Wins)->action_area), button, FALSE, TRUE, 5);
	g_signal_connect_swapped(G_OBJECT(button), "clicked", G_CALLBACK( toCancelWin),GTK_OBJECT(Wins));
	GTK_WIDGET_SET_FLAGS(button, GTK_CAN_DEFAULT);
	gtk_widget_show (button);

	button = create_button(Wins,_("OK"));

	gtk_box_pack_start (GTK_BOX( GTK_DIALOG(Wins)->vbox), table, FALSE, TRUE, 5);

	hbox =addHboxToTable(table, 0, 0, 1, 1);
	hboxSymmetryChargeMultiplicity = hbox;
	createGamessSymmetryFrame(Wins, hbox);

	hbox =addHboxToTable(table, 1, 0, 1, 2);
	initGamessBasisFrame();
	initGamessGuessFrame();
	initGamessSCFFrame();
	createGamessControlFrame(Wins, hbox);
	createGamessBasisFrame(Wins, hbox);
	createGamessChargeMultiplicityFrame(hboxSymmetryChargeMultiplicity);

	hbox =addHboxToTable(table, 2, 0, 1, 2);
	createGamessGuessFrame(Wins, hbox);
	createGamessSCFFrame(Wins, hbox);


	gtk_box_pack_start (GTK_BOX( GTK_DIALOG(Wins)->action_area), button, FALSE, TRUE, 5);
	GTK_WIDGET_SET_FLAGS(button, GTK_CAN_DEFAULT);
	gtk_widget_grab_default(button);
	gtk_widget_show (button);
	g_signal_connect_swapped(G_OBJECT(button), "clicked",G_CALLBACK(putInfoInTextEditor),GTK_OBJECT(Wins));
	g_signal_connect_swapped(G_OBJECT(button), "clicked",G_CALLBACK(destroy_children),GTK_OBJECT(Wins));
	

	gtk_widget_show_all(Wins);
	gamessWin = Wins;
}
/************************************************************************************************************/
void newGamess()
{
	gamessInputFileWindow(TRUE);
}
/************************************************************************************************************/
void insertGamess()
{
	gamessInputFileWindow(FALSE);
}
