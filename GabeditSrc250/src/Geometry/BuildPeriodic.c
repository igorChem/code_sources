/* BuildPeriodic.c */
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
#include <gtk/gtk.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>

#include "../Common/Global.h"
#include "../Utils/Utils.h"
#include "../Utils/UtilsInterface.h"
#include "../Utils/AtomsProp.h"
#include "../Utils/Constants.h"
#include "../Geometry/GeomGlobal.h"
#include "../Geometry/DrawGeom.h"
#include "../Geometry/GeomXYZ.h"
#include "../Geometry/InterfaceGeom.h"
#include "../Geometry/MenuToolBarGeom.h"

void define_good_factor();
void create_GeomXYZ_from_draw_grometry();

#define LigneT 9
#define ColonneT 2

static GtkWidget* Entrys[3];
static GeomDef* G=NULL;
static GeomDef* G0=NULL;
static gint Nb = 0;
static gint Nb0 = 0;
static gint fisrtFragNumber = 0;
static gint nTv = 0;
static gint iTv[3] = {0,0,0};

typedef enum
{
  LEFT1 = 0,
  RIGHT1,
  LEFT2,
  RIGHT2,
  LEFT3,
  RIGHT3
} FragPositions;

/*
static gdouble getTorsion(GeomDef* geom, gint a1, gint a2, gint a3,gint a4)
{
	gdouble C1[3]={geom[a1].X,geom[a1].Y,geom[a1].Z};
	gdouble C2[3]={geom[a2].X,geom[a2].Y,geom[a2].Z};
	gdouble C3[3]={geom[a3].X,geom[a3].Y,geom[a3].Z};
	gdouble C4[3]={geom[a4].X,geom[a4].Y,geom[a4].Z};
	return TorsionToAtoms(C4,C1,C2,C3);
}
*/
/********************************************************************************/
static gboolean isItTv(gchar* symbol)
{
	static gchar tmp[BSIZE];
	sprintf(tmp,"%s",symbol);
	uppercase(tmp);
	if(!strcmp(tmp,"TV")) return TRUE;
	return FALSE;
}
/********************************************************************************/
static void init_variables()
{
	gint j;
	gint i;
	if(Nb!=0)
	{
		gint i;
                for (i=0;i<Nb;i++)
		{
			g_free(G[i].Prop.symbol);
			g_free(G[i].Prop.name);
			g_free(G[i].mmType);
			g_free(G[i].pdbType);
			g_free(G[i].Residue);
			g_free(G[i].typeConnections);
		}

		if(G) g_free(G);
	}
	Nb=0;
	G = NULL;
	if(Natoms<1) return;
	fisrtFragNumber = geometry0[0].ResidueNumber;
	Nb = Natoms;
	G = g_malloc((Nb)*sizeof(GeomDef));
	for(j=0;j<Nb;j++)
	{
		G[j].X=geometry0[j].X;
		G[j].Y=geometry0[j].Y;
		G[j].Z=geometry0[j].Z;
		G[j].Charge=geometry0[j].Charge;
		G[j].mmType=g_strdup(geometry0[j].mmType);
		G[j].pdbType=g_strdup(geometry0[j].pdbType);
		G[j].Residue=g_strdup(geometry0[j].Residue);
		G[j].ResidueNumber=fisrtFragNumber;
		G[j].Prop = prop_atom_get(geometry0[j].Prop.symbol);
		G[j].N = j+1;
		//printf("%s %f %f %f \n",G[j].Prop.symbol,G[j].X,G[j].Y,G[j].Z);
		G[j].typeConnections = NULL;
	}
	nTv = 0;
	for(j=0;j<Nb;j++)
	if(isItTv(G[j].Prop.symbol)) 
	{
		iTv[nTv] = j;
		nTv++;
	}
	Nb0 = Natoms-nTv;
	G0 = g_malloc((Nb0)*sizeof(GeomDef));
	i = 0;
	for(j=0;j<Nb;j++)
	{
		if(isItTv(G[j].Prop.symbol)) continue; 
		G0[i].X=G[j].X;
		G0[i].Y=G[j].Y;
		G0[i].Z=G[j].Z;
		G0[i].Charge=G[j].Charge;
		G0[i].mmType=g_strdup(G[j].mmType);
		G0[i].pdbType=g_strdup(G[j].pdbType);
		G0[i].Residue=g_strdup(G[j].Residue);
		G0[i].ResidueNumber=fisrtFragNumber;
		G0[i].Prop = prop_atom_get(G[j].Prop.symbol);
		G0[i].N = i+1;
		G0[i].typeConnections = NULL;
		i++;
	}
}
/*****************************************************************************/
static void destroy_dlg(GtkWidget* Dlg,gpointer data) 
{
	delete_child(Dlg);
	if(Nb!=0)
	{
		gint i;
		if(G)
                for (i=0;i<Nb;i++)
		{
			g_free(G[i].Prop.symbol);
			g_free(G[i].Prop.name);
			g_free(G[i].mmType);
			g_free(G[i].pdbType);
			g_free(G[i].Residue);
			if(G[i].typeConnections) g_free(G[i].typeConnections);
		}

		if(G) g_free(G);
	}
	Nb=0;
	G = NULL;
	if(Nb0!=0)
	{
		gint i;
		if(G0)
                for (i=0;i<Nb0;i++)
		{
			g_free(G0[i].Prop.symbol);
			g_free(G0[i].Prop.name);
			g_free(G0[i].mmType);
			g_free(G0[i].pdbType);
			g_free(G0[i].Residue);
			if(G0[i].typeConnections) g_free(G0[i].typeConnections);
		}

		if(G0) g_free(G0);
	}
	Nb0=0;
	G0 = NULL;
	
	activate_rotation();
}
/*****************************************************************************/
static void define_geometry_to_draw()
{
	gint i;
	gint j;
	gdouble C[3] = {0.0,0.0,0.0};
	gint n;

	Free_One_Geom(geometry0,Natoms);
	Free_One_Geom(geometry ,Natoms);
	Natoms = 0;
	geometry0 = NULL;
	geometry  = NULL;

	Natoms = Nb;
	if(Natoms<1) return;
	//reset_origine_molecule_drawgeom();
	geometry0 = g_malloc((Natoms)*sizeof(GeomDef));
	geometry  = g_malloc((Natoms)*sizeof(GeomDef));
	n = 0;
	for(i=0;i<Nb;i++)
	{
		geometry0[n].X = G[i].X;
		geometry0[n].Y = G[i].Y;
		geometry0[n].Z = G[i].Z;
		geometry0[n].Charge = G[i].Charge;
		geometry0[n].Prop = prop_atom_get(G[i].Prop.symbol);
		geometry0[n].mmType = g_strdup(G[i].mmType);
		geometry0[n].pdbType = g_strdup(G[i].pdbType);
		geometry0[n].Residue = g_strdup(G[i].Residue);
		geometry0[n].ResidueNumber = G[i].ResidueNumber;
		geometry0[n].show = TRUE;
		geometry0[n].Layer = HIGH_LAYER;
		geometry0[n].Variable = TRUE;


		geometry0[n].N = i+1;
        	geometry0[n].typeConnections = NULL;

		geometry[n].X = G[i].X;
		geometry[n].Y = G[i].Y;
		geometry[n].Z = G[i].Z;
		geometry[n].Charge = G[i].Charge;
		geometry[n].Prop = prop_atom_get(G[i].Prop.symbol);
		geometry[n].mmType = g_strdup(geometry0[n].mmType);
		geometry[n].pdbType = g_strdup(geometry0[n].pdbType);
		geometry[n].Residue = g_strdup(geometry0[n].Residue);
		geometry[n].ResidueNumber = G[i].ResidueNumber;
		geometry[n].show = TRUE;
		geometry[n].N = i+1;
        	geometry[n].typeConnections = NULL;
		geometry[n].Layer = HIGH_LAYER;
		geometry[n].Variable = TRUE;
		C[0] +=  G[i].X;
		C[1] +=  G[i].Y;
		C[2] +=  G[i].Z;
		n++;
	}
	/* center */
/*
	if(n>0) for(i=0;i<3;i++) C[i] /= n;
	for(i=0;i<n;i++)
	{
		geometry0[i].X -= C[0];
		geometry0[i].Y -= C[1];
		geometry0[i].Z -= C[2];

		geometry[i].X -= C[0];
		geometry[i].Y -= C[1];
		geometry[i].Z -= C[2];
	}
*/
	Natoms = n;
	if(n>0)
	{
		geometry0 = g_realloc(geometry0,(Natoms)*sizeof(GeomDef));
		geometry  = g_realloc(geometry,(Natoms)*sizeof(GeomDef));
	}

	for(i=0;i<(gint)Natoms;i++)
	{
		geometry[i].typeConnections = g_malloc(Natoms*sizeof(gint));
		for(j=0;j<(gint)Natoms;j++) geometry[i].typeConnections[j] = 0;
		geometry0[i].typeConnections = g_malloc(Natoms*sizeof(gint));
		for(j=0;j<(gint)Natoms;j++) geometry0[i].typeConnections[j] = 0;
	}
	/*
	for(i=0;i<(gint)Natoms;i++)
	{
		gint iG = geometry[i].N-1;
		for(j=i+1;j<(gint)Natoms;j++) 
		{
			gint jG = geometry[j].N-1;
			geometry[i].typeConnections[j] = G[iG].typeConnections[jG];
			geometry[j].typeConnections[i] = G[jG].typeConnections[iG];
		}
	}
	*/
	for(i=0;i<(gint)Natoms;i++) geometry[i].N = geometry0[i].N = i+1;

	//copy_connections(geometry0,geometry,Natoms);
	RebuildGeom = TRUE;
}
/********************************************************************************/
static gboolean getTvVector(gchar* what, gdouble Tv[])
{
	gint k = 0;
	gboolean Ok = FALSE;
	gint j;
	gdouble orig[3];
	get_origine_molecule_drawgeom(orig);

	if(nTv<1) return FALSE;
	j = iTv[0];
	if(strstr(what,"LEFT 1"))
	{
		Tv[0] = -(G[j].X+orig[0]);
		Tv[1] = -(G[j].Y+orig[1]);
		Tv[2] = -(G[j].Z+orig[2]);
		return TRUE;
	}
	if(strstr(what,"RIGHT 1"))
	{
		Tv[0] = (G[j].X+orig[0]);
		Tv[1] = (G[j].Y+orig[1]);
		Tv[2] = (G[j].Z+orig[2]);
		return TRUE;
	}
	if(nTv<2) return FALSE;

	j = iTv[1];
	if(strstr(what,"LEFT 2"))
	{
		Tv[0] = -(G[j].X+orig[0]);
		Tv[1] = -(G[j].Y+orig[1]);
		Tv[2] = -(G[j].Z+orig[2]);
		return TRUE;
	}
	if(strstr(what,"RIGHT 2"))
	{
		Tv[0] = (G[j].X+orig[0]);
		Tv[1] = (G[j].Y+orig[1]);
		Tv[2] = (G[j].Z+orig[2]);
		return TRUE;
	}
	if(strstr(what,"L1-L2"))
	{
		Tv[0] =-(G[iTv[0]].X+orig[0])-(G[iTv[1]].X+orig[0]);
		Tv[1] =-(G[iTv[0]].Y+orig[1])-(G[iTv[1]].Y+orig[1]);
		Tv[2] =-(G[iTv[0]].Z+orig[2])-(G[iTv[1]].Z+orig[2]);
		return TRUE;
	}
	if(strstr(what,"L1-R2"))
	{
		Tv[0] =-(G[iTv[0]].X+orig[0])+(G[iTv[1]].X+orig[0]);
		Tv[1] =-(G[iTv[0]].Y+orig[1])+(G[iTv[1]].Y+orig[1]);
		Tv[2] =-(G[iTv[0]].Z+orig[2])+(G[iTv[1]].Z+orig[2]);
		return TRUE;
	}
	if(strstr(what,"L2-R1"))
	{
		Tv[0] =(G[iTv[0]].X+orig[0])-(G[iTv[1]].X+orig[0]);
		Tv[1] =(G[iTv[0]].Y+orig[1])-(G[iTv[1]].Y+orig[1]);
		Tv[2] =(G[iTv[0]].Z+orig[2])-(G[iTv[1]].Z+orig[2]);
		return TRUE;
	}
	if(strstr(what,"R1-R2"))
	{
		Tv[0] =(G[iTv[0]].X+orig[0])+(G[iTv[1]].X+orig[0]);
		Tv[1] =(G[iTv[0]].Y+orig[1])+(G[iTv[1]].Y+orig[1]);
		Tv[2] =(G[iTv[0]].Z+orig[2])+(G[iTv[1]].Z+orig[2]);
		return TRUE;
	}

	if(nTv<2) return FALSE;
	j = iTv[2];
	if(strstr(what,"LEFT 3"))
	{
		Tv[0] = -(G[j].X+orig[0]);
		Tv[1] = -(G[j].Y+orig[1]);
		Tv[2] = -(G[j].Z+orig[2]);
		return TRUE;
	}
	if(strstr(what,"RIGHT 3"))
	{
		Tv[0] = (G[j].X+orig[0]);
		Tv[1] = (G[j].Y+orig[1]);
		Tv[2] = (G[j].Z+orig[2]);
		return TRUE;
	}
	if(strstr(what,"L1-L3"))
	{
		Tv[0] =-(G[iTv[0]].X+orig[0])-(G[iTv[2]].X+orig[0]);
		Tv[1] =-(G[iTv[0]].Y+orig[1])-(G[iTv[2]].Y+orig[1]);
		Tv[2] =-(G[iTv[0]].Z+orig[2])-(G[iTv[2]].Z+orig[2]);
		return TRUE;
	}
	if(strstr(what,"L1-R3"))
	{
		Tv[0] =-(G[iTv[0]].X+orig[0])+(G[iTv[2]].X+orig[0]);
		Tv[1] =-(G[iTv[0]].Y+orig[1])+(G[iTv[2]].Y+orig[1]);
		Tv[2] =-(G[iTv[0]].Z+orig[2])+(G[iTv[2]].Z+orig[2]);
		return TRUE;
	}
	if(strstr(what,"L2-L3"))
	{
		Tv[0] =-(G[iTv[1]].X+orig[0])-(G[iTv[2]].X+orig[0]);
		Tv[1] =-(G[iTv[1]].Y+orig[1])-(G[iTv[2]].Y+orig[1]);
		Tv[2] =-(G[iTv[1]].Z+orig[2])-(G[iTv[2]].Z+orig[2]);
		return TRUE;
	}
	if(strstr(what,"L2-R3"))
	{
		Tv[0] =-(G[iTv[1]].X+orig[0])+(G[iTv[2]].X+orig[0]);
		Tv[1] =-(G[iTv[1]].Y+orig[1])+(G[iTv[2]].Y+orig[1]);
		Tv[2] =-(G[iTv[1]].Z+orig[2])+(G[iTv[2]].Z+orig[2]);
		return TRUE;
	}
	if(strstr(what,"L3-R1"))
	{
		Tv[0] =-(G[iTv[2]].X+orig[0])+(G[iTv[0]].X+orig[0]);
		Tv[1] =-(G[iTv[2]].Y+orig[1])+(G[iTv[0]].Y+orig[1]);
		Tv[2] =-(G[iTv[2]].Z+orig[2])+(G[iTv[0]].Z+orig[2]);
		return TRUE;
	}
	if(strstr(what,"L3-R2"))
	{
		Tv[0] =-(G[iTv[2]].X+orig[0])+(G[iTv[1]].X+orig[0]);
		Tv[1] =-(G[iTv[2]].Y+orig[1])+(G[iTv[1]].Y+orig[1]);
		Tv[2] =-(G[iTv[2]].Z+orig[2])+(G[iTv[1]].Z+orig[2]);
		return TRUE;
	}
	if(strstr(what,"R1-R3"))
	{
		Tv[0] =(G[iTv[0]].X+orig[0])+(G[iTv[2]].X+orig[0]);
		Tv[1] =(G[iTv[0]].Y+orig[1])+(G[iTv[2]].Y+orig[1]);
		Tv[2] =(G[iTv[0]].Z+orig[2])+(G[iTv[2]].Z+orig[2]);
		return TRUE;
	}
	if(strstr(what,"R2-R3"))
	{
		Tv[0] =(G[iTv[1]].X+orig[0])+(G[iTv[2]].X+orig[0]);
		Tv[1] =(G[iTv[1]].Y+orig[1])+(G[iTv[2]].Y+orig[1]);
		Tv[2] =(G[iTv[1]].Z+orig[2])+(G[iTv[2]].Z+orig[2]);
		return TRUE;
	}
	return FALSE;
	
}
/********************************************************************************/
static gint getNumFrag(gchar* what)
{
	gint k = 0;
	gboolean Ok = FALSE;
	gint j;
	gint ll;
	if(strstr(what,"LEFT 1")) return fisrtFragNumber+1;
	if(strstr(what,"RIGHT 1")) return fisrtFragNumber+2;
	if(strstr(what,"LEFT 2")) return fisrtFragNumber+3;
	if(strstr(what,"RIGHT 2")) return fisrtFragNumber+4;
	if(strstr(what,"LEFT 3")) return fisrtFragNumber+5;
	if(strstr(what,"RIGHT 3")) return fisrtFragNumber+6;
	ll = fisrtFragNumber+6;
	if(strstr(what,"L1-L2")) return ll+1;
	if(strstr(what,"L1-L3")) return ll+2;
	if(strstr(what,"L1-R2")) return ll+3;
	if(strstr(what,"L1-R3")) return ll+4;
	if(strstr(what,"L2-R1")) return ll+5;
	if(strstr(what,"L2-R3")) return ll+6;
	if(strstr(what,"L2-L3")) return ll+7;
	if(strstr(what,"L3-R1")) return ll+8;
	if(strstr(what,"L3-R2")) return ll+9;
	if(strstr(what,"R1-R2")) return ll+10;
	if(strstr(what,"R1-R3")) return ll+11;
	if(strstr(what,"R2-R3")) return ll+12;
	return ll+13;
}
/********************************************************************************/
static void add_fragment(gchar* what)
{
	gint i;
	gint j;
	gint nAtomsNew= 0;
	gdouble Tv[3];
	gint numFrag = 0;

	G_CONST_RETURN gchar* t;

	if(nTv<1) return;
	nAtomsNew= Nb0;
	if(nAtomsNew<1) return;

	
	if(!getTvVector(what, Tv)) return;
	if(Nb>0) G = g_realloc(G,(Nb+nAtomsNew)*sizeof(GeomDef));
	else G = g_malloc((nAtomsNew)*sizeof(GeomDef));

	Ddef = FALSE;
	//printf("Tv= %f %f %f\n",G[numTv].X,G[numTv].Y,G[numTv].Z);

	numFrag = getNumFrag(what);
	j=Nb-1;
	for(i=0;i<Nb0;i++)
	{
		j++;
		G[j].X=G0[i].X+Tv[0];
		G[j].Y=G0[i].Y+Tv[1];
		G[j].Z=G0[i].Z+Tv[2];
		G[j].Charge=G0[i].Charge;
		G[j].mmType=g_strdup(G0[i].mmType);
		G[j].pdbType=g_strdup(G0[i].pdbType);
		G[j].Residue=g_strdup(G0[i].Residue);
		G[j].ResidueNumber=numFrag;

		G[j].Prop = prop_atom_get(G0[i].Prop.symbol);
		G[j].N = j+1;
		G[j].typeConnections = NULL;
	}
	/*
	G[numTv].X *=2;
	G[numTv].Y *=2;
	G[numTv].Z *=2;
	*/

	Nb += nAtomsNew;

	define_geometry_to_draw();
	//define_good_factor();
	unselect_all_atoms();

	reset_all_connections();

	reset_charges_multiplicities();
	drawGeom();
	create_GeomXYZ_from_draw_grometry();
}
/********************************************************************************/
static void build_periodic(GtkWidget *button,gpointer data)
{
  	GtkWidget* Dlg = g_object_get_data(G_OBJECT (button), "WinDlg");
	gchar fragName[BSIZE];
	sprintf(fragName,"%s",(gchar*)data);
	uppercase(fragName);
	add_fragment(fragName);
}
/********************************************************************************************************/
static void add_buttons(GtkWidget *Dlg,GtkWidget* box)
{
	GtkWidget* Table;
	GtkWidget* button;
	GtkWidget* frame;
	guint i;
	guint j;
        GtkStyle *button_style;
        GtkStyle *style;

	static char *Symb[LigneT][ColonneT]={
		{"Left 1","Right 1"},
		{"Left 2","Right 2"},
		{"Left 3","Right 3"},
		{"L1-L2","L1-L3"},
		{"L1-R2","L1-R3"},
		{"L2-L3","L2-R1"},
		{"L2-R3","L3-R1"},
		{"L3-R2","R1-R2"},
		{"R1-R3","R2-R3"},
		};
  frame = gtk_frame_new (NULL);
  gtk_frame_set_shadow_type( GTK_FRAME(frame),GTK_SHADOW_ETCHED_OUT);
  gtk_container_set_border_width (GTK_CONTAINER (frame), 1);

  gtk_container_add(GTK_CONTAINER(box),frame);  
  gtk_widget_show (frame);

  Table = gtk_table_new(LigneT,ColonneT,TRUE);
  gtk_container_add(GTK_CONTAINER(frame),Table);
  button_style = gtk_widget_get_style(Dlg); 
  
  for ( i = 0;i<LigneT;i++)
	  for ( j = 0;j<ColonneT;j++)
  	{
		gchar tmp[BSIZE];
		sprintf(tmp,"%s",Symb[i][j]);
	  	button = gtk_button_new_with_label(tmp);
  		g_object_set_data(G_OBJECT (button), "WinDlg",Dlg);

          	style=set_button_style(button_style,button,"H");
          	g_signal_connect(G_OBJECT(button), "clicked",(GCallback)build_periodic,(gpointer )Symb[i][j]);
	  	gtk_table_attach(GTK_TABLE(Table),button,j,j+1,i,i+1,
		  (GtkAttachOptions)(GTK_FILL | GTK_EXPAND) ,
		  (GtkAttachOptions)(GTK_FILL | GTK_EXPAND),
		  1,1);
	  }

  g_object_set_data(G_OBJECT (Dlg), "FramePeriodic",frame);
  
}
/**********************************************************************/
void build_periodic_dlg()
{
  GtkWidget *Dlg;
  GtkWidget *Button;
  GtkWidget *hbox;
  GtkWidget *vbox;
  GtkWidget *frame;
  GtkWidget *vboxframe;
  
  init_variables();
  if(nTv == 0 ) 
  {
      	gchar* t = g_strdup_printf(_("Sorry, You must read/build a molecule with at least one Tv vector."));
        GtkWidget* w = Message(t,_("Error"),TRUE);
        g_free(t);
	return;
  }
  Dlg = gtk_dialog_new();
  gtk_window_set_title(GTK_WINDOW(Dlg),_("Build Periodic"));
  gtk_window_set_modal (GTK_WINDOW (Dlg), TRUE);
  gtk_window_set_transient_for(GTK_WINDOW(Dlg),GTK_WINDOW(GeomDlg));


  add_child(GeomDlg,Dlg,gtk_widget_destroy,_(" Build Periodic "));

  g_signal_connect(G_OBJECT(Dlg),"delete_event",(GCallback)destroy_dlg,NULL);

  frame = gtk_frame_new (NULL);
  gtk_frame_set_shadow_type( GTK_FRAME(frame),GTK_SHADOW_ETCHED_OUT);
  gtk_container_set_border_width (GTK_CONTAINER (frame), 2);
  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(Dlg)->vbox), frame,TRUE,TRUE,0);
  gtk_widget_show (frame);

  vboxframe = create_vbox(frame);

  hbox = create_hbox_false(vboxframe);
  add_buttons(Dlg,hbox);

  hbox = create_hbox_false(vboxframe);
  vbox = create_vbox_false(hbox);



  gtk_box_set_homogeneous (GTK_BOX( GTK_DIALOG(Dlg)->action_area), FALSE);
  gtk_widget_realize(Dlg);
  Button = create_button(Dlg,_("Close"));
  gtk_box_pack_end (GTK_BOX( GTK_DIALOG(Dlg)->action_area), Button, FALSE, TRUE, 2);
  g_signal_connect_swapped(G_OBJECT(Button), "clicked",(GCallback)destroy_dlg,GTK_OBJECT(Dlg));

  GTK_WIDGET_SET_FLAGS(Button, GTK_CAN_DEFAULT);
  gtk_widget_grab_default(Button);
  gtk_widget_show_all(GTK_DIALOG(Dlg)->vbox);
  gtk_widget_show_all(GTK_DIALOG(Dlg)->action_area);
  gtk_widget_show_now(Dlg);

  fit_windows_position(GeomDlg, Dlg);

}

