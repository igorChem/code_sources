/* FragmentsTree.c */
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
#include <stdlib.h>
#include "../../Config.h"

#include "../Common/Global.h"
#include "../Utils/Constants.h"
#include "../Utils/UtilsInterface.h"
#include "../Geometry/InterfaceGeom.h"
#include "../Utils/Utils.h"
#include "../Utils/PovrayUtils.h"
#include "../Utils/AtomsProp.h"
#include "../Geometry/GeomGlobal.h"
#include "../Common/Windows.h"
#include "../Geometry/Fragments.h"
#include "../Geometry/DrawGeom.h"
#include "../Geometry/PreviewGeom.h"
#include "../Geometry/PersonalFragments.h"
#include "../Geometry/LoadPersonalFragments.h"

/********************************************************************************/
typedef struct _FragmentGroup FragmentGroup;
typedef struct _FragmentsList FragmentsList;
typedef struct _DataFragTree DataFragTree;
struct _FragmentGroup
{
	gchar* name;
	gint numberOfFragments;
	gchar** fragmentNames;
};
struct _FragmentsList
{
	gint numberOfGroups;
	FragmentGroup* groups;
};
struct _DataFragTree
{
	gint groupNumber;
	gint fragNumber;
};

/********************************************************************************/
#define lengthList	1
static gchar *listTitles[]={ "Fragments"};

typedef enum
{
  LIST_NAME = 0,
  LIST_DATA /* column for data, this column is not visible */
}ListColumnsTypes;
/********************************************************************************/
static void freeFragmentsList(FragmentsList* fragList)
{
	gint i;
	gint j;
	if(!fragList) return;
	if(fragList->numberOfGroups<1)
	{
		fragList->numberOfGroups = 0;
		fragList->groups = NULL;
		return;
	}
	if(fragList->groups)
	for(i=0;i<fragList->numberOfGroups;i++)
	{
		for(j=0;j<fragList->groups[i].numberOfFragments;j++)
		{
			if(fragList->groups[i].fragmentNames[j])
				g_free(fragList->groups[i].fragmentNames[j]);
		}
	}
	if(fragList->groups) g_free(fragList->groups);
	fragList->numberOfGroups = 0;
	fragList->groups = NULL;
}
/********************************************************************************/
static void addOneGroup(FragmentsList* fragList, gchar* groupName, gint nFrag, gchar** fragNames)
{
	gint i;
	gint j;
	if(!fragList) return;
	fragList->numberOfGroups++;
	fragList->groups = g_realloc(fragList->groups, fragList->numberOfGroups*sizeof(FragmentGroup));
	i = fragList->numberOfGroups-1;
	fragList->groups[i].name = g_strdup(groupName);
	fragList->groups[i].numberOfFragments = nFrag;
	fragList->groups[i].fragmentNames = g_malloc(nFrag*sizeof(gchar*));
	for(j=0;j<nFrag;j++)
		fragList->groups[i].fragmentNames[j] = g_strdup(fragNames[j]);

}
/********************************************************************************/
static void addDrugs(FragmentsList* fragList)
{
	static gchar *names[]= {"Viagra","Valium","Morphine",
		"Methadone","LSD","Heroine",
		"Nicotine","Caffeine","Aspirin"};

	if(!fragList) return;
	addOneGroup(fragList, "Drugs", G_N_ELEMENTS(names), names);
}
/********************************************************************************/
static void addRings(FragmentsList* fragList)
{
	static gchar *names[]={
		"Benzene","Benzofuran","Acenaphthene","Xanthene",
		"Norbornane","Porphine","Oxazole","2-Norbornene",
		"Cyclohexene","Cyclohexane","Cyclopentadiene","Cyclopentene",
		"Cyclopentane","Cyclobutene","Cyclobutane","Cyclopropane",
		"Cycloheptene","Cycloheptane","Imidazole"
		};

	if(!fragList) return;
	addOneGroup(fragList, "Rings",  G_N_ELEMENTS (names), names);
}
/********************************************************************************/
static void addFullerenes(FragmentsList* fragList)
{
	static gchar *names[]={
		"C60","C70","C78",
		"C80","C82","C84",
		"C240"
		};

	if(!fragList) return;
	addOneGroup(fragList, "Fullerenes",  G_N_ELEMENTS (names), names);
}
/********************************************************************************/
static void addFunctionals(FragmentsList* fragList)
{
	static gchar *names[]={
		"Methyl","Hydroxy","Carboxylic Acid","Carboxylate",
		"Amine","Amide","Aldehyde","Acid Anhydride",
		"Methoxy","Nitrile","Nitroso","Nitro",
		"Isopropyl","Thiol","Acid Anhydride"
		};

	if(!fragList) return;
	addOneGroup(fragList, "Functionals",  G_N_ELEMENTS (names), names);
}
/********************************************************************************/
static void addHydrocarbon(FragmentsList* fragList)
{
	static gchar *names[]={
			"Propane", "Ethylene", "Ethane", "Methane",
			"Butadiene", "trans-Butane", "cis-Butane", "Propylene",
			"Nonane", "Octane", "Heptane", "Hexane",
			"Tridecane", "Dodecane", "Undecane", "Decane",
			"Heptadecane", "Hexadecane", "Pentadecane", "Tetradecane",
			"Tetracontane", "Eicosane", "Nonadecane", "Octadecane"
		};

	if(!fragList) return;
	addOneGroup(fragList, "Hydrocarbon",  G_N_ELEMENTS (names), names);
}
/********************************************************************************/
static void addMiscellaneous(FragmentsList* fragList)
{
	static gchar *names[]={
		"Water","Imine","Glycerol",
		"Ammonia","Hydrazone","Formamide",
		"Urea","Glycol","Formaldehyde"
		};

	if(!fragList) return;
	addOneGroup(fragList, "Miscellaneous",  G_N_ELEMENTS (names), names);
}
/********************************************************************************/
static void addAminoAcidsL(FragmentsList* fragList)
{
	static gchar *names[]={
		"Ala","Cys","Gly","His","Met","Thr",
		"Arg","Cyx","Cym","Hid","Ile","Phe","Tyr",
		"Asn","Gln","Hie","Leu","Pro","Trp",
		"Asp","Glu","Hip","Lys","Ser","Val",
		"Ash", "Glh","Lyn","Roh",
		"Ace","Nme"
		"Cala","Ccys","Cgly","Chis","Cmet","Cthr",
		"Carg","Ccyx","Ccym","Chid","Cile","Cphe","Ctyr",
		"Casn","Cgln","Chie","Cleu","Cpro","Ctrp",
		"Casp","Cglu","Chip","Clys","Cser","Cval",
		"Cash", "Cglh","Clyn",
		"Nala","Ncys","Ngly","Nhis","Nmet","Nthr",
		"Narg","Ncyx","Ncym","Nhid","Nile","Nphe","Ntyr",
		"Nasn","Ngln","Nhie","Nleu","Npro","Ntrp",
		"Nasp","Nglu","Nhip","Nlys","Nser","Nval",
		"Nash", "Nglh","Nlyn",
		"Oala","Ocys","Ogly","Ohis","Omet","Othr",
		"Oarg","Ocyx","Ocym","Ohid","Oile","Ophe","Otyr",
		"Oasn","Ogln","Ohie","Oleu","Opro","Otrp",
		"Oasp","Oglu","Ohip","Olys","Oser","Oval",
		"Oash", "Oglh","Olyn",
		"Hala","Hcys","Hgly","Hhis","Hmet","Hthr",
		"Harg","Hcyx","Hcym","Hhid","Hile","Hphe","Htyr",
		"Hasn","Hgln","Hhie","Hleu","Hpro","Htrp",
		"Hasp","Hglu","Hhip","Hlys","Hser","Hval",
		"Hash", "Hglh","Hlyn"
		};

	if(!fragList) return;
	addOneGroup(fragList, "Aminoacids(L)",  G_N_ELEMENTS (names), names);
}
/********************************************************************************/
static void addAminoAcidsD(FragmentsList* fragList)
{
	static gchar *names[]={
		"Ala","Cys","Gly","His","Met","Thr",
		"Arg","Cyx","Cym","Hid","Ile","Phe","Tyr",
		"Asn","Gln","Hie","Leu","Pro","Trp",
		"Asp","Glu","Hip","Lys","Ser","Val",
		"Ash", "Glh","Lyn","Roh",
		"Ace","Nme"
		"Cala","Ccys","Cgly","Chis","Cmet","Cthr",
		"Carg","Ccyx","Ccym","Chid","Cile","Cphe","Ctyr",
		"Casn","Cgln","Chie","Cleu","Cpro","Ctrp",
		"Casp","Cglu","Chip","Clys","Cser","Cval",
		"Cash", "Cglh","Clyn",
		"Nala","Ncys","Ngly","Nhis","Nmet","Nthr",
		"Narg","Ncyx","Ncym","Nhid","Nile","Nphe","Ntyr",
		"Nasn","Ngln","Nhie","Nleu","Npro","Ntrp",
		"Nasp","Nglu","Nhip","Nlys","Nser","Nval",
		"Nash", "Nglh","Nlyn",
		"Oala","Ocys","Ogly","Ohis","Omet","Othr",
		"Oarg","Ocyx","Ocym","Ohid","Oile","Ophe","Otyr",
		"Oasn","Ogln","Ohie","Oleu","Opro","Otrp",
		"Oasp","Oglu","Ohip","Olys","Oser","Oval",
		"Oash", "Oglh","Olyn",
		"Hala","Hcys","Hgly","Hhis","Hmet","Hthr",
		"Harg","Hcyx","Hcym","Hhid","Hile","Hphe","Htyr",
		"Hasn","Hgln","Hhie","Hleu","Hpro","Htrp",
		"Hasp","Hglu","Hhip","Hlys","Hser","Hval",
		"Hash", "Hglh","Hlyn"
		};

	if(!fragList) return;
	addOneGroup(fragList, "Aminoacids(D)",  G_N_ELEMENTS (names), names);
}
/********************************************************************************/
static void addAntivirals(FragmentsList* fragList)
{
	static gchar *names[]={
	"Acyclovir" , "Cidofovir" , "Famciclovir",
	"Ganciclovir" , "Idoxuridine" , "Penciclovir",
	"Valacyclovir" , "Vidarabine" ,
		};

	if(!fragList) return;
	addOneGroup(fragList, "Antiviral agents",  G_N_ELEMENTS (names), names);
}
/********************************************************************************/
static void addHeterocyclic(FragmentsList* fragList)
{
	static gchar *names[]={
	"Oxirane" , "Pyrazine" , "Pyridazine" ,
	"Pyridine" , "Pyrimidine" , "Pyrrol" ,
	"Tetrahydrofuran" , "Thiophene"
		};

	if(!fragList) return;
	addOneGroup(fragList, "Heterocyclic",  G_N_ELEMENTS (names), names);
}
/********************************************************************************/
static void addPersonnalFragments(FragmentsList* fragList)
{
	static gchar **names = NULL;
	gchar* fileName = g_strdup_printf("%s%sPersonalFragments.frg",
			gabedit_directory(), G_DIR_SEPARATOR_S);
	gint numberOfGroupes = 0;
	PersonalFragments* personnalFragments = NULL;
	PersonalGroupe* personnalGroupes = NULL;
	gint i;
	gint j;

	personnalFragments = loadAllPersonalFragments(fileName);
	if(personnalFragments) numberOfGroupes = personnalFragments->numberOfGroupes;
	if(personnalFragments) personnalGroupes = personnalFragments->personnalGroupes;

	if(fragList && personnalFragments)
	for(i=0;i<numberOfGroupes;i++)
	{
		if(personnalGroupes[i].numberOfFragments<1) continue;
		names = g_malloc( personnalGroupes[i].numberOfFragments*sizeof(gchar*));
		for(j=0;j<personnalGroupes[i].numberOfFragments;j++)
			names[j] = personnalGroupes[i].fragments[j].name;

		addOneGroup(fragList, 
				personnalFragments->personnalGroupes[i].groupName,
				personnalGroupes[i].numberOfFragments,
				names);
		g_free(names);
	}
	g_free(fileName);
}
/********************************************************************************/
static FragmentsList* getFragmentsList()
{
	FragmentsList* fragList = g_malloc(sizeof(FragmentsList));
	fragList->numberOfGroups = 0;
	fragList->groups = NULL;
	addFunctionals(fragList);
	addRings(fragList);
	addHeterocyclic(fragList);
	addHydrocarbon(fragList);
	addDrugs(fragList);
	addFullerenes(fragList);
	addAminoAcidsL(fragList);
	addAminoAcidsD(fragList);
	addAntivirals(fragList);
	addMiscellaneous(fragList);
	addPersonnalFragments(fragList);
	return fragList;
}
/********************************************************************************/
static void eventDispatcher(GtkWidget *widget, GdkEventButton *event, gpointer user_data)
{
	GtkTreePath *path;
	GtkTreeIter iter;
	GtkTreeModel *model;
	GtkWidget* treeView = widget;

	if (!event) return;
	if (event->window == gtk_tree_view_get_bin_window (GTK_TREE_VIEW (widget))
	    && !gtk_tree_view_get_path_at_pos (GTK_TREE_VIEW (widget), event->x, event->y, NULL, NULL, NULL, NULL)) {
		gtk_tree_selection_unselect_all (gtk_tree_view_get_selection (GTK_TREE_VIEW (widget)));
	}
	if(gtk_tree_view_get_path_at_pos (GTK_TREE_VIEW (widget), event->x, event->y, &path, NULL, NULL, NULL))
	{
		if(path)
		{
   			DataFragTree* data = NULL;
			model = gtk_tree_view_get_model(GTK_TREE_VIEW(widget));
			gtk_tree_selection_select_path  (gtk_tree_view_get_selection (GTK_TREE_VIEW (widget)), path);
			gtk_tree_model_get_iter (model, &iter, path);
			gtk_tree_path_free(path);
			gtk_tree_model_get (model, &iter, LIST_DATA, &data, -1);
			if(data) 
			{
				FragmentsList* fragList = g_object_get_data(G_OBJECT(treeView), "FramentsList");
				if(fragList)
				{
					gint nG = fragList->numberOfGroups;
					gint i = data->groupNumber;
					if(
						i>=0 && i<nG && fragList->groups 
						&& fragList->groups[i].name 
						&& fragList->groups[i].fragmentNames 
					)
					{
						gint j = data->fragNumber;
						gint nF = fragList->groups[i].numberOfFragments;
						if(
							j>=0 && j<nF && fragList->groups[i].fragmentNames[j]
						)
						{
							gchar* tmp = NULL;
							if(strstr(fragList->groups[i].name,"Aminoacids"))
							{
								gchar* dum = g_strdup(fragList->groups[i].fragmentNames[j]);
								lowercase(dum);
								if(strstr(fragList->groups[i].name,"(L)"))
								tmp = g_strdup_printf("%s/L%s",
								fragList->groups[i].name,
								dum);
								else
								tmp = g_strdup_printf("%s/D%s",
								fragList->groups[i].name,
								dum);

								g_free(dum);
							}
							else
							{
								tmp = g_strdup_printf("%s/%s",
								fragList->groups[i].name,
								fragList->groups[i].fragmentNames[j]);
							}
							add_a_fragment(treeView,tmp);
							g_free(tmp);
						}
					}
				}
			}
		}
	}
	GTK_WIDGET_GET_CLASS(widget)->button_press_event(widget, event);
}
/********************************************************************************/
static DataFragTree* newDataFragTree(gint groupNumber, gint fragNumber)
{
	DataFragTree* dataTree;
	dataTree = g_malloc(sizeof(DataFragTree));
	dataTree->groupNumber = groupNumber;
	dataTree->fragNumber = fragNumber;
	return  dataTree;
}
/********************************************************************************/
/*
static void freeDataFragTree(GtkWidget* treeView)
{
   	DataFragTree* data = NULL;
	gint i = 0;
	gchar* pathString = NULL;
	GtkTreeIter iter;
	GtkTreeModel *model = gtk_tree_view_get_model(GTK_TREE_VIEW(treeView));

	pathString = g_strdup_printf("%d", i);
	while (gtk_tree_model_get_iter_from_string (model, &iter, pathString) == TRUE)
	{
		gtk_tree_model_get (model, &iter, LIST_DATA, &data, -1);
		if(data) g_free(data);
		i++;
		g_free(pathString);
		pathString = g_strdup_printf("%d", i);
	}
	g_free(pathString);
}
*/
/********************************************************************************/
/*
static gboolean* getExpandInfo(GtkWidget* treeView)
{
	gint i;
	gboolean* expandeds = NULL;
	gint nNodes = 0;

	gchar* pathString = NULL;
	GtkTreeIter iter;
	GtkTreeModel *model = gtk_tree_view_get_model(GTK_TREE_VIEW(treeView));

	nNodes = 0;
	pathString = g_strdup_printf("%d", nNodes);
	while (gtk_tree_model_get_iter_from_string (model, &iter, pathString) == TRUE)
	{
		nNodes++;
		g_free(pathString);
		pathString = g_strdup_printf("%d", nNodes);
	}
	g_free(pathString);
	if(nNodes<1) return NULL;
	expandeds = g_malloc((nNodes+1)*sizeof(gboolean));
	for(i=0;i<nNodes+1;i++) expandeds[i] = FALSE;

	i = 0;
	pathString = g_strdup_printf("%d", i);
	while (gtk_tree_model_get_iter_from_string (model, &iter, pathString) == TRUE)
	{
		GtkTreePath *path = gtk_tree_path_new_from_string  (pathString);
 		expandeds[i] =  gtk_tree_view_row_expanded(GTK_TREE_VIEW(treeView), path);
		gtk_tree_path_free(path);
		i++;
		g_free(pathString);
		pathString = g_strdup_printf("%d", i);
	}
	g_free(pathString);
	return expandeds;
}
*/
/********************************************************************************/
/*
static void setExpandeds(GtkWidget* treeView, gboolean* expandeds, gchar* selected_row)
{
	gint i;
	gchar* pathString = NULL;
	GtkTreeIter iter;
	GtkTreeModel *model = gtk_tree_view_get_model(GTK_TREE_VIEW(treeView));

	if(!expandeds) return;

	i = 0;
	pathString = g_strdup_printf("%d", i);
	while (gtk_tree_model_get_iter_from_string (model, &iter, pathString) == TRUE)
	{
		if(expandeds[i])
		{
			GtkTreePath *path = gtk_tree_path_new_from_string  (pathString);
			gtk_tree_view_expand_to_path(GTK_TREE_VIEW(treeView), path);
			gtk_tree_path_free(path);
		}
		i++;
		g_free(pathString);
		pathString = g_strdup_printf("%d", i);
	}
	g_free(pathString);
	if(selected_row)
	{
			GtkTreePath *path = gtk_tree_path_new_from_string  (selected_row);
			gtk_tree_selection_select_path  (gtk_tree_view_get_selection (GTK_TREE_VIEW (treeView)), path);
			gtk_tree_path_free(path);
	}
}
*/
/*******************************************************************************************************/
static void addFeuille(GtkWidget* treeView, GtkTreeIter *parent, gchar* fragName, gint groupNumber, gint fragNumber)
{
	GtkTreeIter feuille;
	DataFragTree* dataTree;
	GtkTreeModel *model;
        GtkTreeStore *store;

	model = gtk_tree_view_get_model(GTK_TREE_VIEW(treeView));
        store = GTK_TREE_STORE (model);


	dataTree = newDataFragTree(groupNumber,fragNumber);

	gtk_tree_store_append(store, &feuille, parent);
       	gtk_tree_store_set (store, &feuille, LIST_NAME, fragName, -1);
       	gtk_tree_store_set (store, &feuille, LIST_DATA, dataTree, -1);
}

/********************************************************************************/
static GtkTreeIter addNode(GtkWidget* treeView, gchar *text,gint atomNumber)
{
	GtkTreeIter node;
	DataFragTree* dataTree;
	GtkTreeModel *model;
        GtkTreeStore *store;

	model = gtk_tree_view_get_model(GTK_TREE_VIEW(treeView));
        store = GTK_TREE_STORE (model);

	gtk_tree_store_append(store, &node, NULL);
	dataTree = newDataFragTree(atomNumber,-1);
       	gtk_tree_store_set (store, &node, LIST_NAME, text, -1);
       	gtk_tree_store_set (store, &node, LIST_DATA, dataTree, -1);

	return node;
}
/***********************************************************************/
static void addFragmentsList(GtkWidget *treeView, FragmentsList* list)
{
	gint i;
	GtkTreeIter node;
	gint j;
	
	for(i=0;i<list->numberOfGroups;i++)
	{
		node = addNode(treeView, list->groups[i].name,i);

		for(j=0;j<list->groups[i].numberOfFragments;j++)
		{
			addFeuille(treeView, &node,list->groups[i].fragmentNames[j],i,j);
		}

	}
}
/********************************************************************************/
static void clearTreeView(GtkWidget* treeView)
{
	GtkTreeModel *model = gtk_tree_view_get_model(GTK_TREE_VIEW(treeView));
        GtkTreeStore *store = GTK_TREE_STORE (model);
	gtk_tree_store_clear(store);
}
/********************************************************************************/
void rafreshTreeView(GtkWidget *treeView)
{
	FragmentsList* fragList = NULL;
	if(treeView == NULL) return;

	fragList = g_object_get_data(G_OBJECT(treeView), "FramentsList");
	if(fragList) freeFragmentsList(fragList);
	fragList = getFragmentsList();
	clearTreeView(treeView);
	addFragmentsList(treeView, fragList);
	g_object_set_data(G_OBJECT(treeView), "FramentsList", fragList);
}
/***********************************************************************/
GtkWidget* addFragmentsTreeView(GtkWidget *box)
{
	GtkWidget *scr;
	gint i;
        GtkTreeStore *store;
	GtkTreeModel *model;
	GtkCellRenderer *renderer;
	GtkTreeViewColumn *column;
	GtkWidget* treeView;
	GtkWidget* drawingArea = NULL;

	scr = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scr), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC); 
	gtk_container_add(GTK_CONTAINER(box), scr);      

	store = gtk_tree_store_new (lengthList+1, G_TYPE_STRING, G_TYPE_POINTER);
        model = GTK_TREE_MODEL (store);

	treeView = gtk_tree_view_new_with_model (model);
	g_object_set_data(G_OBJECT(box), "FramentsTreeView", treeView);

	drawingArea = g_object_get_data(G_OBJECT(box), "DrawingArea");

	gtk_tree_view_set_rules_hint (GTK_TREE_VIEW (treeView), TRUE);
	gtk_tree_view_set_headers_visible (GTK_TREE_VIEW (treeView), TRUE);

	for (i=0;i<lengthList;i++)
	{
		column = gtk_tree_view_column_new ();
		gtk_tree_view_column_set_title (column, listTitles[i]);
		renderer = gtk_cell_renderer_text_new ();
		gtk_tree_view_column_pack_start (column, renderer, FALSE);
		gtk_tree_view_column_set_attributes (column, renderer, "text", i, NULL);
		gtk_tree_view_append_column (GTK_TREE_VIEW (treeView), column);
	}

	gtk_container_add(GTK_CONTAINER(scr), treeView);
	if(drawingArea) g_object_set_data(G_OBJECT(treeView), "DrawingArea", drawingArea);
	g_signal_connect(treeView, "button_press_event", G_CALLBACK(eventDispatcher), NULL);
	rafreshTreeView(treeView);
	return treeView;
}
/***********************************************************************/
