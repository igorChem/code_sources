/* RingsOrb.c */
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
#include "GlobalOrb.h"
#include "BondsOrb.h"
#include "../Utils/Vector3d.h"
#include "../Utils/Transformation.h"
#include "../Utils/Utils.h"
#include "../Utils/UtilsGL.h"
#include "../Utils/UtilsInterface.h"
#include "../Utils/Constants.h"
#include "../Utils/HydrogenBond.h"
#include "../Display/RingsPov.h"
#include "../Display/UtilsOrb.h"

/************************************************************************/
typedef struct 
{
	gchar name[10];
	gdouble thetas[3];
}ConformerTypes;

typedef struct 
{
	gchar desc[1000];
	gdouble thetas[3];
	gdouble rms;
}ConformerTypes2;


/************************************************************************/
static V4d color_r[7] = 
{
	{1.0, 1.0,1.0,0.8},
	{0.0, 1.0,1.0,0.8},
	{1.0, 0.0,1.0,0.8},
	{1.0, 1.0,0.0,0.8},
	{0.0, 1.0,0.0,0.8},
	{0.0, 0.0,1.0,0.8},
	{1.0, 0.0,0.0,0.8}
};

static gint nAtoms = 0;
static gint **connected = NULL;
static gboolean *inStack = NULL;
static gboolean done = FALSE;
static gint bonds = 0;
static gint rSize = 0;
static gboolean deleteNotPlaner = FALSE;
static gboolean randumColors = FALSE;
static V4d randumC = {1.0, 1.0,1.0,0.8};
static gdouble epsilonCoplaner = 5; /* in degre */
/************************************************************************/
gboolean ringsGetRandumColors()
{
	return randumColors;
}
/************************************************************************/
void ringsSetRandumColors(gboolean ac)
{
	randumColors = ac;
}
/************************************************************************/
gboolean ringsGetNotPlanar()
{
	return deleteNotPlaner;
}
/************************************************************************/
void ringsSetNotPlanar(gboolean ac)
{
	deleteNotPlaner = ac;
}
/************************************************************************/
static void freeStack()
{
	if(inStack) g_free(inStack);
	inStack = NULL;
}
/************************************************************************/
static void initStack()
{
	gint i;
	if(inStack) freeStack();
	inStack = g_malloc(Ncenters*sizeof(gboolean));
	for(i=0;i<Ncenters;i++) inStack[i] = FALSE;
}
/************************************************************************/
static void freeConnections()
{
	gint i;
	if(!connected) return;
	for(i=0;i<nAtoms;i++)
	{
		if(connected[i]) g_free(connected[i]);
	}
	g_free(connected);
	connected = NULL;
	nAtoms = 0;
}
/************************************************************************/
void buildConnectionsForRings()
{
	GList* list;
	gint i = 0;
	gint k = 0;
	if(!BondsOrb) return;
	freeConnections();
	nAtoms = Ncenters;
	connected = g_malloc(nAtoms*sizeof(gint*));
	for(i=0;i<nAtoms;i++)
	{
		connected[i] = g_malloc((Ncenters+1)*sizeof(gint));
		connected[i][0] = 0;
	}
	for(list=BondsOrb;list!=NULL;list=list->next)
	{
		BondType* data=(BondType*)list->data;
		gint i = data->n1;
		gint j = data->n2;

		connected[i][0]++;
		connected[j][0]++;

		k = connected[i][0];
		connected[i][k]=j;

		k = connected[j][0];
		connected[j][k]=i;

	}
}
/************************************************************************/
gboolean inRing(gint currentAtom, gint rootAtom, gint ringSize, gboolean initialize)
{
	gint i;
	if (initialize)
	{
		done = FALSE;
		bonds = 0;
		rSize = ringSize;
		initStack();
		if(nAtoms != Ncenters) buildConnectionsForRings();
		if(Ncenters<1) return FALSE;
	}
	else 
		inStack[currentAtom] = TRUE;
	if (done) return TRUE;
	else if ( ( currentAtom == rootAtom ) && ( bonds == ringSize ) ) return TRUE;
	else if ( ( currentAtom == rootAtom ) && ( bonds > 2 ) && ( ringSize < 3 ) ) return TRUE;
	if ( bonds < ringSize )
	{
		gint numberOfConnections = connected[ currentAtom ][ 0 ];
		for (i = 1; i <= numberOfConnections; i++ )
		{
			gint newAtom = connected[currentAtom][i];
			if ( ! ( inStack[newAtom] ) )
			{
				bonds++;
				done = inRing( newAtom, rootAtom, ringSize, FALSE );
			}
			if (done) return TRUE;
		}
	}
	inStack[currentAtom] = FALSE;
	bonds--;
	return FALSE;
}
/************************************************************************/
static gboolean isConnected(gint i, gint j)
{
	gint k;
	for(k=0;k<connected[i][0];k++)
		if(connected[i][k+1]==j) return TRUE;
	return FALSE;
}
/************************************************************************/
gint* getRing()
{
	gint i;
	gint n= 0;
 	gint* ringAtoms = NULL;
	gint k;
	gint j;
	if(inStack && rSize>1)
	{
 		ringAtoms = g_malloc(rSize*sizeof(gint));
		for(i=0;i<nAtoms;i++)
		{
			if(inStack[i])
			{
				ringAtoms[n] = i;
				n++;
				if(n>=rSize) break;
			}
		}
	}
	if(ringAtoms)
	{
		for(i=1;i<n;i++)
			if(ringAtoms[i]<ringAtoms[0])
			{
				gint t = ringAtoms[i];
				ringAtoms[i] = ringAtoms[0];
				ringAtoms[0] = t;
			}

		for(i=0;i<n-2;i++)
		{
			k = i+1;
			for(j=i+1;j<n;j++)
				if(isConnected(ringAtoms[i],ringAtoms[j]))
				{
					k = j;
					break;
				}
			if(k!=(i+1))
			{
				gint t = ringAtoms[i+1];
				ringAtoms[i+1] = ringAtoms[k];
				ringAtoms[k] = t;
			}
		}
	}
	return ringAtoms;
}
/********************************************************************************/
void getCentreRing(gint i, gint j, gdouble C[])
{

	gint k;
	gint c;
	gint* num  = NULL;
	gint n;

	for(c=0;c<3;c++) C[c] = 0;
	n = 4;
	if(inRing(j, i, n-1, TRUE) ) num = getRing();
	if(!num)
	{
		n++;
		if(inRing(j, i, n-1, TRUE)) num = getRing();
	}
	if(!num)
	{
		n++;
		if(inRing(j, i, n-1, TRUE)) num = getRing();
	}
	if(num)
	{
		for(c=0;c<3;c++) C[c] += GeomOrb[j].C[c];
		for(k=0;k<n-1;k++) 
			for(c=0;c<3;c++) C[c] += GeomOrb[num[k]].C[c];
		for(c=0;c<3;c++) C[c] /= n;
		if(num)g_free(num);
	}
}
/********************************************************************************/
/*
static gint** freeRings(gint** ringsAtoms, gint* nRings)
{
	gint k;
	if(!ringsAtoms) return NULL;
	for(k=0;k<*nRings;k++)
		if(ringsAtoms[k]) g_free(ringsAtoms[k]);
	g_free(ringsAtoms);
	*nRings=0;
	return NULL;
}
*/
/************************************************************************/
static gint** addRing(gint** ringsAtoms, gint* nRings, gint ringSize, gint* ringAtoms)
{
	gint k;
	gint i;
	if(!ringAtoms) return ringsAtoms;
	if(*nRings==0)
	{
		ringsAtoms = g_malloc(sizeof(gint*));
		k = 0;
		ringsAtoms[k] = g_malloc(ringSize*sizeof(gint));
		for(i=0;i<ringSize;i++)
			ringsAtoms[k][i] = ringAtoms[i];
		(*nRings)++;
		return ringsAtoms;
	}
	for(k=0;k<*nRings;k++)
	{
		gboolean Ok = TRUE;
		for(i=0;i<ringSize;i++)
			if(ringsAtoms[k][i] != ringAtoms[i])
			{
				Ok = FALSE;
				break;
			}
		if(Ok) return ringsAtoms;
	}
	(*nRings)++;
	ringsAtoms = g_realloc(ringsAtoms, (*nRings)*sizeof(gint*));
	k = *nRings-1;
	ringsAtoms[k] = g_malloc(ringSize*sizeof(gint));
	for(i=0;i<ringSize;i++)
		ringsAtoms[k][i] = ringAtoms[i];
	return ringsAtoms;
}
/************************************************************************/
gint** getRings(gint ringSize, gint* nRings)
{
	gint n1;
	gint numAtom = 0;
	gint** ringsAtoms = NULL;
	*nRings = 0;
	if(Ncenters<1) return NULL;
	buildConnectionsForRings();
	for(numAtom=0;numAtom<Ncenters;numAtom++)
	if(inRing(numAtom, numAtom, ringSize, TRUE)) 
	{
		gint* ringAtoms = NULL;
		ringAtoms = getRing();
		if(ringAtoms)
		{
			ringsAtoms = addRing(ringsAtoms, nRings,  ringSize, ringAtoms);
			g_free(ringAtoms);
		}
	}
	for(n1=0;n1<Ncenters;n1++)
	{
		gint j;
		gint numberOfConnections = connected[ n1 ][ 0 ];
		for (j = 2; j <= numberOfConnections; j++ )
		{
			gint* ringAtoms = NULL;
			gint t = connected[ n1 ][ 1 ];
			connected[ n1 ][ 1 ] =  connected[ n1 ][ j ]; 
			connected[ n1 ][ j ] = t;
			if(!inRing(n1, n1, ringSize, TRUE)) continue;
			ringAtoms = getRing();
			if(ringAtoms)
			{
				ringsAtoms = addRing(ringsAtoms, nRings,  ringSize, ringAtoms);
				g_free(ringAtoms);
			}
		}
	}


	return ringsAtoms;
}
/********************************************************************************/
static gint nRings=0;
static GList** rings=NULL;
static gint ringSizeMax = 6;
static gint ringSizeMin = 3;
static gint* ringsSize = NULL;
/************************************************************************/
static gint cmp_fonction(gconstpointer a, gconstpointer b)
{
		if( GPOINTER_TO_INT(a)== GPOINTER_TO_INT(b)) return 0;
		if( GPOINTER_TO_INT(a)> GPOINTER_TO_INT(b)) return 1;
		return -1;
}
/************************************************************************/
static gboolean compare2List(GList* L1, GList* L2)
{
	GList*ll1 = g_list_copy(L1);
	GList*ll2 = g_list_copy(L2);
	GList*l1 = NULL;
	GList*l2 = NULL;
	gboolean Ok;
	ll1 = g_list_sort(ll1, (GCompareFunc)cmp_fonction);
	ll2 = g_list_sort(ll2, (GCompareFunc)cmp_fonction);
	Ok = TRUE;
	for(l1=ll1, l2=ll2; l1 != NULL && l2 != NULL ; l1 = l1->next, l2 = l2->next)
	{
		if(GPOINTER_TO_INT(l1->data) != GPOINTER_TO_INT(l2->data))
		{
			Ok = FALSE;
			break;
		}
	}
	g_list_free(ll1);
	g_list_free(ll2);
	if(l1 != NULL || l2 != NULL) Ok = FALSE;
	return Ok;
}
/************************************************************************/
static gboolean isCoplanar(GList* ring, gdouble epsilon) /* epsilon on degre */
{
	GList*l = NULL;
	V3d normal = {0,0,0};
	V3d v = {0,0,0};
	V3d v1 = {0,0,0};
	V3d v2 = {0,0,0};
	V3d p[] = {{0,0,0}, {0,0,0}, {0,0,0} };
	gint  i;
	gint  j;
	gint  k;
	gdouble tol = 0.1;

	tol = cos((90-epsilon)/180.0*PI);

	for(l=ring, j=0; j<3 ; l = l->next, j++)
	{
		if(!l) return TRUE;
		i = GPOINTER_TO_INT(l->data);
		for(k=0;k<3;k++) p[j][k] = GeomOrb[i].C[k];
	}
	for(k=0;k<3;k++) v1[k] = p[1][k] - p[0][k];
	for(k=0;k<3;k++) v2[k] = p[2][k] - p[0][k];
	v3d_cross(v1,v2, normal);
	v3d_normal(normal);
	for( ; l != NULL ; l = l->next)
	{
		i = GPOINTER_TO_INT(l->data);
		for(k=0;k<3;k++) v[k] = GeomOrb[i].C[k]-p[0][k];
		v3d_normal(v);
		if(fabs(v3d_dot(normal, v))>tol) return FALSE;

	}
	return TRUE;
}
/************************************************************************/
static GList** deleteNotCorrectRingsSize(gint* nR, GList** rings)
{
	gint n;
	gint n1;
	gint i;
	gint nRings = *nR;
	for(n=0;n<nRings;n++)
	{
		if(ringsSize[n]<ringSizeMin) 
		{
			g_list_free(rings[n]);
			rings[n] = NULL;
		}
	}
	i = 0;
	for(n=0;n<nRings;n++)
	{
		if(ringsSize[n]>=ringSizeMin) { i++; continue;}
		for(n1=n+1;n1<nRings;n1++)
			if(rings[n1] != NULL)
			{
				GList* t= rings[n1];
				gint k = ringsSize[n1];
				rings[n1] = rings[n];
				rings[n] = t;

				ringsSize[n1] = ringsSize[n];
				ringsSize[n] = k;
				i++;
				break;
			}
	}
	nRings = i;
	if(i>0) rings = g_realloc(rings, nRings*sizeof(GList*));
	else if(rings) {g_free(rings); rings = NULL; nRings = 0;}
	*nR = nRings;
	return rings;
}
/************************************************************************/
static GList** deleteNotRings(gint* nR, GList** rings, gboolean deleteNotPlaner)
{
	gint n;
	gint n1;
	gint i;
	gint nRings = *nR;
	if(nRings<1) return rings;
	if(!rings) return rings;
	if(ringsSize) g_free(ringsSize);
	ringsSize = g_malloc(nRings*sizeof(gint));
	for(n=0;n<nRings;n++)
	{
		GList* glist = rings[n];
		GList* l = NULL;
		GList* l2 = NULL;
		gint begin;
		gint end;

		ringsSize[n] = 0;
		if(!glist) continue;
		begin = GPOINTER_TO_INT(glist->data);
		end = begin;
		for(l=glist; l != NULL; l = l->next)
		{
			ringsSize[n]++;
			end = GPOINTER_TO_INT(l->data);
			for(l2=l->next; l2 != NULL; l2 = l2->next)
			{
				i= GPOINTER_TO_INT(l2->data);
				if(i==end) 
				{
					ringsSize[n] = -1;
					break;
				}
			}
			if(ringsSize[n]<0) break;
		}
		if(!isConnected(begin, end)) ringsSize[n] = -1; /* this is not a ring */
	}
	rings = deleteNotCorrectRingsSize(&nRings, rings);
	
	for(n=0;n<nRings;n++)
	{
		for(n1=n+1;n1<nRings;n1++)
		{
			if(ringsSize[n] != ringsSize[n1]) continue;
			if(compare2List(rings[n], rings[n1]))
			{
				ringsSize[n] = -1; /* mark for deleting */
				break;
			}
		}
	}
	rings = deleteNotCorrectRingsSize(&nRings, rings);

	if(deleteNotPlaner)
	{
		for(n=0;n<nRings;n++)
			if(!isCoplanar(rings[n], epsilonCoplaner))
				ringsSize[n] = -1; /* mark for deleting */
		rings = deleteNotCorrectRingsSize(&nRings, rings);
	}

	for(n=0;n<nRings;n++)
	{
		GList* l = NULL;
		GList* l0 = NULL;
		GList* l1 = NULL;
		gint i1;
		gint i2;
		for(l=rings[n]; l != NULL; l = l->next)
		{
			i1= GPOINTER_TO_INT(l->data);
			l0 = l->next;
			if(l0 != NULL) l0 = l0->next;
			for(l1=l0; l1 != NULL; l1 = l1->next)
			{
				i2 = GPOINTER_TO_INT(l1->data);
				if(isConnected(i1,i2)) 
				{
					if(l1->next != NULL ) ringsSize[n] = -1;
					else if( l!= rings[n]) ringsSize[n] = -1;
				}
			}
		}
	}
	rings = deleteNotCorrectRingsSize(&nRings, rings);

	*nR = nRings;
	return rings;


}
/************************************************************************/
gboolean findRings(gint currentAtom, gint rootAtom, gint ringIndex, gint depth, gboolean initialize)
{

	gint i;
	gint numberOfConnections;
	gboolean endSearch =TRUE;
	gboolean e =TRUE;
	GList* first;
	GList* l;
	if (initialize)
	{
		initStack();
		if(rings) g_free(rings);
		nRings = 1;
		rings = g_malloc(sizeof(GList*));
		rings[0] = NULL;
	}
	inStack[currentAtom] = TRUE;
	for(l=rings[ringIndex]; l != NULL; l = l->next)
			if(GPOINTER_TO_INT(l->data)== currentAtom) return TRUE;

	rings[ringIndex] = g_list_append(rings[ringIndex],GINT_TO_POINTER(currentAtom));
	if ( !initialize &&  ( depth>=ringSizeMax-1 ) ) return TRUE; /* max ring */
	if ( !initialize &&  ( currentAtom == rootAtom ) ) return TRUE; /* end of cycle */
	if( connected[ currentAtom ][ 0 ]==0) return TRUE; /* end of tree */

	numberOfConnections = connected[ currentAtom ][ 0 ];
	first = g_list_copy(rings[ringIndex]);
	for (i = 1; i <= numberOfConnections; i++ )
	{
		gint newAtom = connected[currentAtom][i];
		if(inStack[newAtom]) continue;
		rings = g_realloc(rings,(nRings+1)*sizeof(GList*));
		rings[nRings] = g_list_copy(first);
		nRings++;
		e = findRings( newAtom, rootAtom, nRings-1, depth+1, FALSE);
		if(!e) endSearch = FALSE;
	}
	inStack[currentAtom] = FALSE;
	for (i = 1; i <= numberOfConnections; i++ )
	{
		gint newAtom = connected[currentAtom][i];
		inStack[newAtom] = FALSE;
	}
	
	return endSearch;
}
/************************************************************************/
static void printRings(gint nRings, GList** rings)
{
	gint i;
	for(i=0;i<nRings;i++)
	{
		GList* glist = rings[i];
		GList* l = NULL;
		if(ringsSize[i]<ringSizeMin) continue;
		printf("Ring number %d : ",i+1);
		for(l=glist; l != NULL; l = l->next)
			printf("%d ",1+GPOINTER_TO_INT(l->data));
		printf("\n");
	}
}
/************************************************************************/
void findAllRingsForOneAtom(gint numAtom)
{
	if(Ncenters<1) return;
	buildConnectionsForRings();
	findRings(numAtom, numAtom, 0, 0, TRUE);
	rings = deleteNotRings(&nRings, rings, FALSE);
	printRings(nRings, rings);
}
/************************************************************************/
static GList** findAllRings(gint* nR, gint ringMinSize, gint ringMaxSize, gboolean deleteNotPlaner)
{
	gint numAtom;
	gint nRingsAll = 0;
	GList** ringsAll = NULL;
	gint n0 = 0;
	gint i;

	*nR = 0;
	if(Ncenters<1) return NULL;
	ringSizeMax = ringMaxSize;
	ringSizeMin = ringMinSize;
	buildConnectionsForRings();
	for(numAtom=0;numAtom<Ncenters;numAtom++)
	{
		findRings(numAtom, numAtom, 0, 0, TRUE);
		rings = deleteNotRings(&nRings, rings, deleteNotPlaner);
		/* printRings(nRings, rings);*/
		if(nRings<1) continue;
		nRingsAll += nRings;
		if(ringsAll)
			ringsAll = g_realloc(ringsAll, nRingsAll*sizeof(GList*));
		else
			ringsAll = g_malloc(nRingsAll*sizeof(GList*));
		n0 = nRingsAll - nRings;
		for(i=0;i<nRings;i++)
		{
			ringsAll[n0+i] = rings[i];
		}
	}
	ringsAll = deleteNotRings(&nRingsAll, ringsAll, deleteNotPlaner);
	/* printRings(nRingsAll, ringsAll);*/
	*nR = nRingsAll;
	return ringsAll;
}
/********************************************************************************/
static void messagesNumberOfRings(gint nRings, GList** rings)
{
	gint i;
	gint* nR;
	gint n = ringSizeMax-ringSizeMin+1;
	gchar buffer[BSIZE];
	gchar buffer1[BSIZE];
	if(n<1) return ;
	nR = g_malloc(n*sizeof(gint));
	for(i=0;i<n;i++)nR[i] = 0;
	for(i=0;i<nRings;i++) nR[ringsSize[i]-ringSizeMin]++;
	for(i=0;i<n;i++)
	{
		if(i+ringSizeMin==3) sprintf(buffer1,"triangles");
		else if(i+ringSizeMin==4) sprintf(buffer1,"rectangles");
		else if(i+ringSizeMin==5) sprintf(buffer1,"pentagons");
		else if(i+ringSizeMin==6) sprintf(buffer1,"hexagons");
		else if(i+ringSizeMin==7) sprintf(buffer1,"heptagons");
		else if(i+ringSizeMin==8) sprintf(buffer1,"octagons");
		else sprintf(buffer1,"rings of size = %d\n",i+ringSizeMin);
		if(i==0)
		sprintf(buffer,"I found %d %s\n",nR[i], buffer1);
		else
		sprintf(buffer + strlen(buffer),"%s is %d\n", buffer1, nR[i]);
	}
	g_free(nR);
	Message(buffer,"Info",TRUE);

}
/********************************************************************************/
void IsoRingsAllGenLists(GLuint *myList, gint ringSizeMin, gint ringSizeMax)
{
	gdouble* Diffuse  = color_r[0];
	gdouble* Specular = color_r[0];
	gdouble* Ambiant  = color_r[0];
	gint k;
	gint nRings = 0;
	gint ringSize = 0;
	GList** rings = NULL;
	GList* l = NULL;
	gint n;

	if (glIsList(*myList) == GL_TRUE) glDeleteLists(*myList,1);
	rings = findAllRings(&nRings, ringSizeMin, ringSizeMax, deleteNotPlaner);

	if(!rings || nRings <1)
	{
		*myList=0;
		messagesNumberOfRings(0, NULL);
		return;
	}

    	*myList = glGenLists(1);
	glNewList(*myList, GL_COMPILE);

	glLineWidth(1.5);
	glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);

	if(TypeBlend == GABEDIT_BLEND_YES) glEnable(GL_BLEND);
	else glDisable(GL_BLEND);

	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);


	for(n=0;n<nRings;n++)
	{
		
		/*
		if(n>0) break;
		printRings(1, &rings[n]);
		*/

		ringSize = ringsSize[n];
		if(ringSize<=6)
		{
			if(!randumColors)
			{
				Diffuse = color_r[ringSize];
				Specular = color_r[ringSize];
				Ambiant = color_r[ringSize];
			}
			else
			{
				randumC[0] = rand()/(double)RAND_MAX;
				randumC[1] = rand()/(double)RAND_MAX;
				randumC[2] = rand()/(double)RAND_MAX;
				Diffuse = randumC;
				Specular = randumC;
				Ambiant = randumC;
			}
		}
		else
		{
			Diffuse = color_r[0];
			Specular = color_r[0];
			Ambiant = color_r[0];
		}
		glMaterialdv(GL_FRONT_AND_BACK,GL_SPECULAR,Specular);
		glMaterialdv(GL_FRONT_AND_BACK,GL_DIFFUSE,Diffuse);
		glMaterialdv(GL_FRONT_AND_BACK,GL_AMBIENT,Ambiant);
		glMateriali(GL_FRONT_AND_BACK,GL_SHININESS,120);

		glBegin(GL_POLYGON);
		for(l = rings[n]; l != NULL; l = l->next)
		{
			k = GPOINTER_TO_INT(l->data);
			glNormal3f(0.0,0.0,1.0);
			glVertex3f(GeomOrb[k].C[0],GeomOrb[k].C[1],GeomOrb[k].C[2]);
		}
		k = GPOINTER_TO_INT(rings[n]->data);;
		glNormal3f(0.0,0.0,1.0);
		glVertex3f(GeomOrb[k].C[0],GeomOrb[k].C[1],GeomOrb[k].C[2]);

		glEnd();
	}
	glEndList();

	if(TypeBlend == GABEDIT_BLEND_YES) glDisable(GL_BLEND);
	messagesNumberOfRings(nRings, rings);
	AddRingsPovRay(rings, nRings, ringsSize, color_r);

	for(n=0;n<nRings;n++)
		if(rings[n]) g_list_free(rings[n]);
	if(rings) g_free(rings);
	if(ringsSize) g_free(ringsSize);
	ringsSize = NULL;

}
/********************************************************************************/
void IsoRingsAllShowLists(GLuint myList)
{
	if(SurfShow == GABEDIT_SURFSHOW_NO) return;
	if (glIsList(myList) == GL_TRUE) glCallList(myList);

}
/********************************************************************************/
static void vCross(gdouble* v1, gdouble* v2, gdouble* cross)
{
    cross[0] = (v1[1] * v2[2]) - (v1[2] * v2[1]);
    cross[1] = (v1[2] * v2[0]) - (v1[0] * v2[2]);
    cross[2] = (v1[0] * v2[1]) - (v1[1] * v2[0]);
}
static gdouble vLength(gdouble* v)
{
    return sqrt(v[0] * v[0] + v[1] * v[1] + v[2] * v[2]);
}
static gdouble vDot(gdouble* v1, gdouble* v2)
{
    return v1[0]*v2[0] + v1[1]*v2[1] + v1[2]*v2[2];
}
static gdouble getOneRMS(ConformerTypes listConformers[], gint numConfos, gdouble thetas[], gint nMax)
{
	gdouble rms = 0;
	gint i;
	for(i=0;i<nMax;i++) 
	{
		rms += (thetas[i]-listConformers[numConfos].thetas[i])*(thetas[i]-listConformers[numConfos].thetas[i]);
	}
	rms /= nMax;
	rms = sqrt(rms);
	return rms;

}
static gint addListRMS(ConformerTypes listConformers[], gint nConfos, gint nAtoms, gint numAtoms[], ConformerTypes2 listRMS[], gint nOldRMS)
{
	gint nMax = nAtoms-3;
	gdouble a[nMax][3];
	gdouble r[nAtoms][3];
	gdouble p[nAtoms][3];
	gdouble q[nMax][3];
	gdouble n[3];
	gint i;
	gdouble nLength = 1;
	gdouble qLength = 1;
	gint rmsIMin;
	gdouble rmsMin;
	gchar* tmp1 = g_malloc(200*sizeof(gchar));
	gchar* tmp2 = g_malloc(200*sizeof(gchar));
	gdouble thetas[nMax];
	gint c;

	
	for(i=0;i<nMax;i++)
	{
		gint twoi=numAtoms[2*i];
		gint twoip1=numAtoms[(2*(i+1))%nAtoms];
		for(c=0;c<3;c++) a[i][c] = GeomOrb[twoip1].C[c]-GeomOrb[twoi].C[c];
	}
	for(i=0;i<nAtoms;i++)
	{
		gint ki=numAtoms[i];
		gint kip1=numAtoms[(i+1)%nAtoms];
		for(c=0;c<3;c++) r[i][c] = GeomOrb[kip1].C[c]-GeomOrb[ki].C[c];
	}
	for(i=1;i<nAtoms;i++) vCross(r[i-1],r[i], p[i]);
	for(i=0;i<nMax;i++) vCross(a[i],p[2*i+1],q[i]);
	vCross(a[1],a[0],n);
	
	nLength = vLength(n);
	if(nLength>1e-10) nLength = 1/nLength;
	for(i=0;i<nMax;i++) 
	{
		qLength = vLength(q[i]);
		if(qLength>1e-10) qLength = 1/qLength;
		thetas[i] = M_PI/2-acos(vDot(q[i],n)*qLength*nLength);
		thetas[i] = thetas[i]/M_PI*180.0;
		while(thetas[i]>=360) thetas[i] -= 360;
		if(thetas[i]>180) thetas[i] = 180-thetas[i];
	}
	sprintf(tmp1,"%s"," ");
	for(i=0;i<nAtoms;i++)
	{
		gint k = numAtoms[i];
		if(nAtoms<10) sprintf(tmp2,"%s%s[%d] ",tmp1,GeomOrb[k].Symb,k+1);
		else if(nAtoms<100) sprintf(tmp2,"%s%s[%2d] ",tmp1,GeomOrb[k].Symb,k+1);
		else if(nAtoms<1000) sprintf(tmp2,"%s%s[%3d] ",tmp1,GeomOrb[k].Symb,k+1);
		else sprintf(tmp2,"%s%s[%d] ",tmp1,GeomOrb[k].Symb,k+1);
		sprintf(tmp1,"%s",tmp2);
	}
	/*
	for(i=0;i<nMax;i++) 
	{
		sprintf(tmp2,"%sthetas[%d]=%f ",tmp1,i,thetas[i]);
		sprintf(tmp1,"%s",tmp2);
	}
	printf("tmp2=%s\n",tmp2);
	*/
	for(i=0;i<nConfos;i++)
	{
		gdouble rms;
		gint k;
		rms = getOneRMS(listConformers, i, thetas, nMax);
		sprintf(listRMS[nOldRMS+i].desc,"%s\t%-6s\t\tRMS=%0.6f",tmp2,listConformers[i].name, rms);
		listRMS[nOldRMS+i].rms = rms;
		for(k=0;k<nMax;k++) listRMS[nOldRMS+i].thetas[k] = thetas[k];
	}
	if(tmp1) g_free(tmp1);
	if(tmp2) g_free(tmp2);
	return nOldRMS+nConfos;
}
static void sortListRMS(ConformerTypes2 listRMS[], gint nRMS)
{
	gint i;
        for(i=0;i<nRMS-1;i++)
	{
		gint k = i;
		gint j;
             	for(j=i+1;j<nRMS;j++) if(listRMS[k].rms>listRMS[j].rms) k = j;
             	if(k!=i)
             	{
               		ConformerTypes2 t = listRMS[i];
                   	listRMS[i] = listRMS[k];
                   	listRMS[k] = t;
             	}
	}
}
static void printListRMS(ConformerTypes2 listRMS[], gint nRMS, gdouble cutOff)
{
	gint i;
        for(i=0;i<nRMS;i++)
	{
		if(i==0 || listRMS[i].rms<cutOff) 
		{
			printf("%s                 Thetas = %0.6f %0.6f %0.6f\n", 
			        listRMS[i].desc,            listRMS[i].thetas[0],listRMS[i].thetas[1],listRMS[i].thetas[2]);
		}
	}
}
/************************************************************************/
static void showListRMS(ConformerTypes2 listRMS[], gint nRMS, gdouble cutOff, gchar* title)
{
	gchar* old;
	gchar* result;
	gint i;

	result = g_strdup_printf("%s",
	"===========================================================================\n"
	"Type of conformation calculated using the method given in\n"
	"Anthony D Hill and Peter J. Reilly J. Chem. Inf. Model, 47 (2007) 1031-1035\n"
	"===========================================================================\n"
	);
        for(i=0;i<nRMS;i++)
        {
                if(i==0 || listRMS[i].rms<cutOff)
                {
			old = result;
			 result = g_strdup_printf("%s%s\t\t\tThetas = %0.6f %0.6f %0.6f\n",
                                                   old,listRMS[i].desc,   listRMS[i].thetas[0],listRMS[i].thetas[1],listRMS[i].thetas[2]);
                         if(old) g_free(old);
                }
        }
        if(result)
        {
                GtkWidget* message = MessageTxt(result,title);
                gtk_window_set_default_size (GTK_WINDOW(message),(gint)(ScreenWidth*0.8),-1);
                gtk_widget_set_size_request(message,(gint)(ScreenWidth*0.45),-1);
                /* gtk_window_set_modal (GTK_WINDOW (message), TRUE);*/
                gtk_window_set_transient_for(GTK_WINDOW(message),GTK_WINDOW(PrincipalWindow));
        }

}
/********************************************************************************/
static void setPropForOneType(ConformerTypes* listConformers, gint i, gchar* name, gdouble t0, gdouble t1, gdouble t2)
{
        sprintf(listConformers[i].name,"%s",name);
        listConformers[i].thetas[0] = t0;
        listConformers[i].thetas[1] = t1;
        listConformers[i].thetas[2] = t2;
}
static ConformerTypes* initListOfConformers(gint* pnTypes)
{
	gint nTypes = 100;
	gint n = -1;
	ConformerTypes* listConformes = g_malloc(nTypes*sizeof(ConformerTypes));
	
	n++; setPropForOneType(listConformes,n,"1C4", -35.260000, -35.260000, -35.260000);
	n++; setPropForOneType(listConformes,n,"4C1", 35.260000, 35.260000, 35.260000);
	n++; setPropForOneType(listConformes,n,"1,4B", -35.260000, 74.200000, -35.260000);
	n++; setPropForOneType(listConformes,n,"B1,4", 35.260000, -74.200000, 35.260000);
	n++; setPropForOneType(listConformes,n,"2,5B", 74.200000, -35.260000, -35.260000);
	n++; setPropForOneType(listConformes,n,"B2,5", -74.200000, 35.260000, 35.260000);
	n++; setPropForOneType(listConformes,n,"3,6B", -35.260000, -35.260000, 74.200000);
	n++; setPropForOneType(listConformes,n,"B3,6", 35.260000, 35.260000, -74.200000);
	n++; setPropForOneType(listConformes,n,"1H2", -42.160000, 9.070000, -17.830000);
	n++; setPropForOneType(listConformes,n,"2H1", 42.160000, -9.070000, 17.830000);
	n++; setPropForOneType(listConformes,n,"2H3", 42.160000, 17.830000, -9.060000);
	n++; setPropForOneType(listConformes,n,"3H2", -42.160000, -17.830000, 9.060000);
	n++; setPropForOneType(listConformes,n,"3H4", -17.830000, -42.160000, 9.070000);
	n++; setPropForOneType(listConformes,n,"4H3", 17.830000, 42.160000, -9.070000);
	n++; setPropForOneType(listConformes,n,"4H5", -9.070000, 42.160000, 17.830000);
	n++; setPropForOneType(listConformes,n,"5H4", 9.070000, -42.160000, -17.830000);
	n++; setPropForOneType(listConformes,n,"5H6", 9.070000, -17.830000, -42.160000);
	n++; setPropForOneType(listConformes,n,"6H5", -9.070000, 17.830000, 42.160000);
	n++; setPropForOneType(listConformes,n,"6H1", 17.830000, -9.070000, 42.160000);
	n++; setPropForOneType(listConformes,n,"1H6", -17.830000, 9.070000, -42.160000);
	n++; setPropForOneType(listConformes,n,"1S3", 0.000000, 50.840000, -50.840000);
	n++; setPropForOneType(listConformes,n,"3S1", 0.000000, -50.840000, 50.840000);
	n++; setPropForOneType(listConformes,n,"5S1", 50.840000, -50.840000, 0.000000);
	n++; setPropForOneType(listConformes,n,"1S5", -50.840000, 50.840000, 0.000000);
	n++; setPropForOneType(listConformes,n,"6S2", -50.840000, 0.000000, 50.840000);
	n++; setPropForOneType(listConformes,n,"2S6", 50.840000, 0.000000, -50.840000);
	n++; setPropForOneType(listConformes,n,"1E", -35.260000, 17.370000, -35.260000);
	n++; setPropForOneType(listConformes,n,"E1", 35.260000, -17.370000, 35.260000);
	n++; setPropForOneType(listConformes,n,"2E", 46.860000, 0.000000, 0.000000);
	n++; setPropForOneType(listConformes,n,"E2", -46.860000, 0.000000, 0.000000);
	n++; setPropForOneType(listConformes,n,"3E", -35.260000, -35.260000, 17.370000);
	n++; setPropForOneType(listConformes,n,"E3", 35.260000, 35.260000, -17.370000);
	n++; setPropForOneType(listConformes,n,"4E", 0.000000, 46.860000, 0.000000);
	n++; setPropForOneType(listConformes,n,"E4", 0.000000, -46.860000, 0.000000);
	n++; setPropForOneType(listConformes,n,"5E", 17.370000, -35.260000, -35.260000);
	n++; setPropForOneType(listConformes,n,"E5", -17.370000, 35.260000, 35.260000);
	n++; setPropForOneType(listConformes,n,"6E", 0.000000, 0.000000, 46.860000);
	n++; setPropForOneType(listConformes,n,"E6", 0.000000, 0.000000, -46.860000);

	*pnTypes = n;
	listConformes = g_realloc(listConformes, nTypes*sizeof(ConformerTypes));
	return listConformes;

}
static void permutationRight(gint numAtoms[], gint nAtoms)
{
	gint zero=numAtoms[nAtoms-1];
	gint i;
	for(i=nAtoms-1;i>=1;i--) numAtoms[i]=numAtoms[i-1];
	numAtoms[0] = zero;
}
/*
static void permutationLeft(gint numAtoms[], gint nAtoms)
{
	gint end=numAtoms[0];
	gint i;
	for(i=0;i<nAtoms-1;i++) numAtoms[i]=numAtoms[i+1];
	numAtoms[nAtoms-1] = end;
}
*/
static void permutationInvers(gint numAtoms[], gint nAtoms)
{
	gint i;
	for(i=1;i<nAtoms/2;i++) 
	{
		gint t = numAtoms[i];
		numAtoms[i]=numAtoms[nAtoms-i];
		numAtoms[nAtoms-i] = t;
		
	}
}
static void setOAtEnd(gint numAtoms[], gint nAtoms)
{
	gint i;
	gint k;
	for(i=0;i<nAtoms;i++) 
	{
		k = numAtoms[nAtoms-1];
                if(!strcmp(GeomOrb[k].Symb,"O")) break;
		permutationRight(numAtoms, nAtoms);
	}
}
static gboolean permutationSugar(gint numAtoms[], gint nAtoms)
{
	gint i;
	gint nO = 0;
	gint k;
	gint numberOfConnections;
	gint currentAtom;
	gint nC;
	//printf("nAtoms = %d\n",nAtoms);
	for(i=0;i<nAtoms;i++) 
	{
		k = numAtoms[i];
                if(!strcmp(GeomOrb[k].Symb,"O")) nO++;
	}
	if(nO==0) return FALSE;
	//printf("nO = %d\n",nO);

	setOAtEnd(numAtoms, nAtoms);
	k = numAtoms[nAtoms-1];
	if(strcmp(GeomOrb[k].Symb,"O")) return FALSE;
	buildConnectionsForRings();
	currentAtom = numAtoms[0];
	numberOfConnections = connected[ currentAtom ][ 0 ];
	nC = 0;
	//printf("numberOfConnections = %d\n",numberOfConnections);
        for (i = 1; i <= numberOfConnections; i++ )
        {
              k = connected[currentAtom][i];
		//printf("k = %d\n",k);
              if(!strcmp(GeomOrb[k].Symb,"C")) nC++;
        }
	//printf("nC = %d\n",nC);
	if(nC<2) return TRUE;
	permutationInvers(numAtoms,nAtoms);
	setOAtEnd(numAtoms, nAtoms);
	return TRUE;
}
/* Ref Anthony D Hill and Peter J. Reilly J. Chem. Inf. Model, 47 (2007) 1031-1035 */
/********************************************************************************/
void computeConformerType()
{
	
	gint ringSizeMin = 6;
	gint ringSizeMax = 6;
	gint k;
	gint nRings = 0;
	gint ringSize = 0;
	GList** rings = NULL;
	GList* l = NULL;
	gint n;
	gboolean deleteNotPlaner = FALSE;
	gint numAtoms[ringSizeMax];
	gdouble thetas[ringSizeMax];
	ConformerTypes* listConformers = NULL;
	gint nConfos = 0;
	gint i;
	gdouble rms;

	rings = findAllRings(&nRings, ringSizeMin, ringSizeMax, deleteNotPlaner);

	if(!rings || nRings <1)
	{
		//messagesNumberOfRings(0, NULL);
		return;
	}
	listConformers = initListOfConformers(&nConfos);

	for(n=0;n<nRings;n++)
	{
		gint nRMS = 0;
		/*
		if(n>0) break;
		printRings(1, &rings[n]);
		*/

		ringSize = ringsSize[n];
		ConformerTypes2* listRMS=g_malloc(ringSize*2*nConfos*sizeof(ConformerTypes2));
		//printf("Ring number %d\n",n+1);
		i=0;
		for(l = rings[n]; l != NULL; l = l->next)
		{
			k = GPOINTER_TO_INT(l->data);
			//printf("%s[%d] ",GeomOrb[k].Symb,k+1);
			numAtoms[i] = k; 
			i++;
		}
		//printf("\n");
		for(i=0;i<ringSize;i++)
		{
			nRMS = addListRMS(listConformers, nConfos, ringSize, numAtoms, listRMS, nRMS);
			permutationRight(numAtoms, ringSize);
			//printf("End addListRMS nRMS = %d\n",nRMS);
		}
		permutationInvers(numAtoms, ringSize);
		for(i=0;i<ringSize;i++)
		{
			nRMS = addListRMS(listConformers, nConfos, ringSize, numAtoms, listRMS, nRMS);
			permutationRight(numAtoms, ringSize);
			//printf("End addListRMS nRMS = %d\n",nRMS);
		}

		sortListRMS(listRMS, nRMS);
		//printf("End sortListRMS\n");
		printListRMS(listRMS, nRMS, 20.0);// all rms<20
		//printf("End printListRMS\n");
		showListRMS(listRMS, nRMS, 20,"Result");
		//printf("End showListRMS\n");

		if(permutationSugar(numAtoms, ringSize))
		{
			nRMS = 0;
			nRMS = addListRMS(listConformers, nConfos, ringSize, numAtoms, listRMS, nRMS);
			sortListRMS(listRMS, nRMS);
			showListRMS(listRMS, nRMS, 1e10,"Result for a saccharide");
			gint i;
                	printf("Saccharide detected : ");
			for(i=0;i<ringSize;i++)
        		{
                		gint k = numAtoms[i];
                		printf("%s[%d] ",GeomOrb[k].Symb,k+1);
        		}
                	printf("\n");
		}
		if(listRMS) g_free(listRMS);
		nRMS = 0;
	}
	//messagesNumberOfRings(nRings, rings);

	for(n=0;n<nRings;n++)
		if(rings[n]) g_list_free(rings[n]);
	if(rings) g_free(rings);
	if(ringsSize) g_free(ringsSize);
	if(listConformers) g_free(listConformers);
	nConfos = 0;
	ringsSize = NULL;
}
/********************************************************************************/
