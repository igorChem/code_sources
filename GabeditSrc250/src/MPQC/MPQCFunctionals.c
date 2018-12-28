/* MPQCFunctionals.c */
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
#include <ctype.h>

#include "../../Config.h"
#include "../Common/Global.h"
#include "../MPQC/MPQCTypes.h"
#include "../MPQC/MPQCGlobal.h"
#include "../Utils/UtilsInterface.h"
#include "../Utils/Constants.h"

/************************************************************************************************************/
void initMPQCFunctionals()
{
	gint n = G96X+1;
	gint i;
	functionals = g_malloc(n*sizeof(MPQCFunctional));
	sumFunctionals = g_malloc(n*sizeof(gdouble));;
	for(i=0;i<n;i++)
		sumFunctionals[i] = 0.0;
	sumFunctionals[SlaterX] = 1.0;

	functionals[LSDAC].type = LSDAC;
	functionals[LSDAC].name = g_strdup("LSDACFunctional");
	functionals[LSDAC].comment = g_strdup(_("Nothing"));

	functionals[PBEC].type = PBEC;
	functionals[PBEC].name = g_strdup("PBECFunctional");
	functionals[PBEC].comment = g_strdup(_("Perdew-Burke-Ernzerhof (PBE) correlation functional."));

	functionals[PW91C].type = PW91C;
	functionals[PW91C].name = g_strdup("PW91CFunctional");
	functionals[PW91C].comment = g_strdup(_("Perdew-Wang 1991 correlation functional"));

	functionals[P86C].type = P86C;
	functionals[P86C].name = g_strdup("P86CFunctional");
	functionals[P86C].comment = g_strdup(_("Perdew 1986 (P86) correlation functional"));

	functionals[NewP86C].type = NewP86C;
	functionals[NewP86C].name = g_strdup("P86CFunctional");
	functionals[NewP86C].comment = g_strdup(_("Perdew 1986 (P86) correlation functional"));


	functionals[VWN1LC].type = VWN1LC;
	functionals[VWN1LC].name = g_strdup("VWN1LCFunctional");
	functionals[VWN1LC].comment = g_strdup(_("VWN1 local correlation term (from Vosko, Wilk, and Nusair)"));

	functionals[VWN1LCRPA].type = VWN1LCRPA;
	functionals[VWN1LCRPA].name = g_strdup("VWN1LCFunctional(RPA)");
	functionals[VWN1LCRPA].comment = g_strdup(_("VWN1(RPA) local correlation term (from Vosko, Wilk, and Nusair)"));

	functionals[VWN2LC].type = VWN2LC;
	functionals[VWN2LC].name = g_strdup("VWN2LCFunctional");
	functionals[VWN2LC].comment = g_strdup(_("VWN2 local correlation term (from Vosko, Wilk, and Nusair)"));

	functionals[VWN3LC].type = VWN3LC;
	functionals[VWN3LC].name = g_strdup("VWN3LCFunctional");
	functionals[VWN3LC].comment = g_strdup(_("VWN3 local correlation term (from Vosko, Wilk, and Nusair)"));

	functionals[VWN4LC].type = VWN4LC;
	functionals[VWN4LC].name = g_strdup("VWN4LCFunctional");
	functionals[VWN4LC].comment = g_strdup(_("VWN4 local correlation term (from Vosko, Wilk, and Nusair)"));

	functionals[VWN5LC].type = VWN5LC;
	functionals[VWN5LC].name = g_strdup("VWN5LCFunctional");
	functionals[VWN5LC].comment = g_strdup(_("VWN5 local correlation term (from Vosko, Wilk, and Nusair)"));

	functionals[PW92LC].type = PW92LC ;
	functionals[PW92LC].name = g_strdup("PW92LCFunctional");
	functionals[PW92LC].comment = g_strdup(_("PW92 local (LSDA) correlation term"));

	functionals[PZ81LC].type =PZ81LC ;
	functionals[PZ81LC].name = g_strdup("PZ81LCFunctional");
	functionals[PZ81LC].comment = g_strdup(_("PZ81 local (LSDA) correlation functional"));

	functionals[LYPC].type = LYPC;
	functionals[LYPC].name = g_strdup("LYPCFunctional");
	functionals[LYPC].comment = g_strdup(_("Lee, Yang, and Parr correlation functional"));

	functionals[HFX].type = HFX;
	functionals[HFX].name = g_strdup("HFX(HF Exchange)");
	functionals[HFX].comment = g_strdup(_("Hartree-Fock Exchange"));

	functionals[Xalpha].type = Xalpha;
	functionals[Xalpha].name = g_strdup("XalphaFunctional");
	functionals[Xalpha].comment = g_strdup(_("Xalpha exchange functional"));

	functionals[SlaterX].type = SlaterX ;
	functionals[SlaterX].name = g_strdup("SlaterXFunctional");
	functionals[SlaterX].comment = g_strdup(_("Slater Exchange term"));

	functionals[Becke88X].type = Becke88X ;
	functionals[Becke88X].name = g_strdup("Becke88XFunctional");
	functionals[Becke88X].comment = g_strdup(_("Becke's 1988 exchange functional"));

	functionals[PBEX].type = PBEX;
	functionals[PBEX].name = g_strdup("PBEXFunctional");
	functionals[PBEX].comment = g_strdup(_("Perdew-Burke-Ernzerhof (PBE) exchange functional"));

	functionals[PW86X].type = PW86X;
	functionals[PW86X].name = g_strdup("PW86XFunctional");
	functionals[PW86X].comment = g_strdup(_("Perdew-Wang 1986 (PW86) Exchange functiona"));

	functionals[PW91X].type = PW91X ;
	functionals[PW91X].name = g_strdup("PW91XFunctional");
	functionals[PW91X].comment = g_strdup(_("Perdew-Wang 1991 exchange functional"));

	functionals[mPW91_B88X].type = mPW91_B88X ;
	functionals[mPW91_B88X].name = g_strdup("mPW91XFunctional(B88)");
	functionals[mPW91_B88X].comment = g_strdup(_("modified 1991 Perdew-Wang exchange functional"));

	functionals[mPW91_PW91X].type =  mPW91_PW91X;
	functionals[mPW91_PW91X].name = g_strdup("mPW91XFunctional(PW91)");
	functionals[mPW91_PW91X].comment = g_strdup(_("modified 1991 Perdew-Wang exchange functional"));

	functionals[mPW91_mPW91X].type =  mPW91_mPW91X;
	functionals[mPW91_mPW91X].name = g_strdup("mPW91XFunctional(mPW91)");
	functionals[mPW91_mPW91X].comment = g_strdup(_("modified 1991 Perdew-Wang exchange functional"));

	functionals[G96X].type = G96X;
	functionals[G96X].name = g_strdup("G96XFunctional");
	functionals[G96X].comment = g_strdup(_("Gill 1996 (G96) exchange functional"));
}
/************************************************************************************************************/
void freeMPQCFunctionals()
{
	gint n = G96X+1;
	gint i;
	if(!functionals)return;
	for(i=0;i<n;i++)
	{
		if(functionals[i].name)g_free(functionals[i].name);
		if(functionals[i].comment)g_free(functionals[i].comment);
	}
	g_free(functionals);
	if(sumFunctionals) g_free(sumFunctionals);
}
/************************************************************************************************************/
void initMPQCStdFunctionals()
{
	gint n = mPW1PW91+1;
	stdFunctionals = g_malloc(n*sizeof(MPQCStdFunctional));

	stdFunctionals[XALPHA].type = XALPHA;
	stdFunctionals[XALPHA].name = g_strdup("XALPHA");
	stdFunctionals[XALPHA].n = 1;
	stdFunctionals[XALPHA].listOfTypes = g_malloc(stdFunctionals[XALPHA].n*sizeof(MPQCFunctionalType));
	stdFunctionals[XALPHA].listOfTypes[0]= Xalpha;
	stdFunctionals[XALPHA].coefficients = g_malloc(stdFunctionals[XALPHA].n*sizeof(gdouble));
	stdFunctionals[XALPHA].coefficients[0]= 1.0;

	stdFunctionals[HFS].type = HFS;
	stdFunctionals[HFS].name = g_strdup("HFS");
	stdFunctionals[HFS].n = 1;
	stdFunctionals[HFS].listOfTypes = g_malloc(stdFunctionals[HFS].n*sizeof(MPQCFunctionalType));
	stdFunctionals[HFS].listOfTypes[0]= SlaterX;
	stdFunctionals[HFS].coefficients = g_malloc(stdFunctionals[HFS].n*sizeof(gdouble));
	stdFunctionals[HFS].coefficients[0]= 1.0;

	stdFunctionals[HFB].type = HFB;
	stdFunctionals[HFB].name = g_strdup("HFB");
	stdFunctionals[HFB].n = 1;
	stdFunctionals[HFB].listOfTypes = g_malloc(stdFunctionals[HFB].n*sizeof(MPQCFunctionalType));
	stdFunctionals[HFB].listOfTypes[0]= Becke88X;
	stdFunctionals[HFB].coefficients = g_malloc(stdFunctionals[HFB].n*sizeof(gdouble));
	stdFunctionals[HFB].coefficients[0]= 1.0;

	stdFunctionals[HFG96].type = HFG96;
	stdFunctionals[HFG96].name = g_strdup("HFG96");
	stdFunctionals[HFG96].n = 1;
	stdFunctionals[HFG96].listOfTypes = g_malloc(stdFunctionals[HFG96].n*sizeof(MPQCFunctionalType));
	stdFunctionals[HFG96].listOfTypes[0]= G96X;
	stdFunctionals[HFG96].coefficients = g_malloc(stdFunctionals[HFG96].n*sizeof(gdouble));
	stdFunctionals[HFG96].coefficients[0]= 1.0;

	stdFunctionals[G96LYP].type = G96LYP;
	stdFunctionals[G96LYP].name = g_strdup("G96LYP");
	stdFunctionals[G96LYP].n = 2;
	stdFunctionals[G96LYP].listOfTypes = g_malloc(stdFunctionals[G96LYP].n*sizeof(MPQCFunctionalType));
	stdFunctionals[G96LYP].listOfTypes[0]= G96X;
	stdFunctionals[G96LYP].listOfTypes[1]= LYPC;
	stdFunctionals[G96LYP].coefficients = g_malloc(stdFunctionals[G96LYP].n*sizeof(gdouble));
	stdFunctionals[G96LYP].coefficients[0]= 1.0;
	stdFunctionals[G96LYP].coefficients[1]= 1.0;


	stdFunctionals[BLYP].type = BLYP;
	stdFunctionals[BLYP].name = g_strdup("BLYP");
	stdFunctionals[BLYP].n = 3;
	stdFunctionals[BLYP].listOfTypes = g_malloc(stdFunctionals[BLYP].n*sizeof(MPQCFunctionalType));
	stdFunctionals[BLYP].listOfTypes[0]= SlaterX;
	stdFunctionals[BLYP].listOfTypes[1]= Becke88X;
	stdFunctionals[BLYP].listOfTypes[2]= LYPC;
	stdFunctionals[BLYP].coefficients = g_malloc(stdFunctionals[BLYP].n*sizeof(gdouble));
	stdFunctionals[BLYP].coefficients[0]= 1.0;
	stdFunctionals[BLYP].coefficients[1]= 1.0;
	stdFunctionals[BLYP].coefficients[2]= 1.0;

	stdFunctionals[SVWN1].type = SVWN1;
	stdFunctionals[SVWN1].name = g_strdup("SVWN1");
	stdFunctionals[SVWN1].n = 2;
	stdFunctionals[SVWN1].listOfTypes = g_malloc(stdFunctionals[SVWN1].n*sizeof(MPQCFunctionalType));
	stdFunctionals[SVWN1].listOfTypes[0]= SlaterX;
	stdFunctionals[SVWN1].listOfTypes[1]= VWN1LC;
	stdFunctionals[SVWN1].coefficients = g_malloc(stdFunctionals[SVWN1].n*sizeof(gdouble));
	stdFunctionals[SVWN1].coefficients[0]= 1.0;
	stdFunctionals[SVWN1].coefficients[1]= 1.0;


	stdFunctionals[SVWN1RPA].type = SVWN1RPA;
	stdFunctionals[SVWN1RPA].name = g_strdup("SVWN1RPA");
	stdFunctionals[SVWN1RPA].n = 2;
	stdFunctionals[SVWN1RPA].listOfTypes = g_malloc(stdFunctionals[SVWN1RPA].n*sizeof(MPQCFunctionalType));
	stdFunctionals[SVWN1RPA].listOfTypes[0]= SlaterX;
	stdFunctionals[SVWN1RPA].listOfTypes[1]= VWN1LCRPA;
	stdFunctionals[SVWN1RPA].coefficients = g_malloc(stdFunctionals[SVWN1RPA].n*sizeof(gdouble));
	stdFunctionals[SVWN1RPA].coefficients[0]= 1.0;
	stdFunctionals[SVWN1RPA].coefficients[1]= 1.0;

	stdFunctionals[SVWN2].type = SVWN2;
	stdFunctionals[SVWN2].name = g_strdup("SVWN2");
	stdFunctionals[SVWN2].n = 2;
	stdFunctionals[SVWN2].listOfTypes = g_malloc(stdFunctionals[SVWN2].n*sizeof(MPQCFunctionalType));
	stdFunctionals[SVWN2].listOfTypes[0]= SlaterX;
	stdFunctionals[SVWN2].listOfTypes[1]= VWN2LC;
	stdFunctionals[SVWN2].coefficients = g_malloc(stdFunctionals[SVWN2].n*sizeof(gdouble));
	stdFunctionals[SVWN2].coefficients[0]= 1.0;
	stdFunctionals[SVWN2].coefficients[1]= 1.0;

	stdFunctionals[SVWN3].type = SVWN3;
	stdFunctionals[SVWN3].name = g_strdup("SVWN3");
	stdFunctionals[SVWN3].n = 2;
	stdFunctionals[SVWN3].listOfTypes = g_malloc(stdFunctionals[SVWN3].n*sizeof(MPQCFunctionalType));
	stdFunctionals[SVWN3].listOfTypes[0]= SlaterX;
	stdFunctionals[SVWN3].listOfTypes[1]= VWN3LC;
	stdFunctionals[SVWN3].coefficients = g_malloc(stdFunctionals[SVWN3].n*sizeof(gdouble));
	stdFunctionals[SVWN3].coefficients[0]= 1.0;
	stdFunctionals[SVWN3].coefficients[1]= 1.0;

	stdFunctionals[SVWN4].type = SVWN4;
	stdFunctionals[SVWN4].name = g_strdup("SVWN4");
	stdFunctionals[SVWN4].n = 2;
	stdFunctionals[SVWN4].listOfTypes = g_malloc(stdFunctionals[SVWN4].n*sizeof(MPQCFunctionalType));
	stdFunctionals[SVWN4].listOfTypes[0]= SlaterX;
	stdFunctionals[SVWN4].listOfTypes[1]= VWN4LC;
	stdFunctionals[SVWN4].coefficients = g_malloc(stdFunctionals[SVWN4].n*sizeof(gdouble));
	stdFunctionals[SVWN4].coefficients[0]= 1.0;
	stdFunctionals[SVWN4].coefficients[1]= 1.0;

	stdFunctionals[SVWN5].type = SVWN5;
	stdFunctionals[SVWN5].name = g_strdup("SVWN5");
	stdFunctionals[SVWN5].n = 2;
	stdFunctionals[SVWN5].listOfTypes = g_malloc(stdFunctionals[SVWN5].n*sizeof(MPQCFunctionalType));
	stdFunctionals[SVWN5].listOfTypes[0]= SlaterX;
	stdFunctionals[SVWN5].listOfTypes[1]= VWN5LC;
	stdFunctionals[SVWN5].coefficients = g_malloc(stdFunctionals[SVWN5].n*sizeof(gdouble));
	stdFunctionals[SVWN5].coefficients[0]= 1.0;
	stdFunctionals[SVWN5].coefficients[1]= 1.0;

	stdFunctionals[SPZ81].type = SPZ81;
	stdFunctionals[SPZ81].name = g_strdup("SPZ81");
	stdFunctionals[SPZ81].n = 2;
	stdFunctionals[SPZ81].listOfTypes = g_malloc(stdFunctionals[SPZ81].n*sizeof(MPQCFunctionalType));
	stdFunctionals[SPZ81].listOfTypes[0]= SlaterX;
	stdFunctionals[SPZ81].listOfTypes[1]= PZ81LC;
	stdFunctionals[SPZ81].coefficients = g_malloc(stdFunctionals[SPZ81].n*sizeof(gdouble));
	stdFunctionals[SPZ81].coefficients[0]= 1.0;
	stdFunctionals[SPZ81].coefficients[1]= 1.0;

	stdFunctionals[SPW92].type = SPW92;
	stdFunctionals[SPW92].name = g_strdup("SPW92");
	stdFunctionals[SPW92].n = 2;
	stdFunctionals[SPW92].listOfTypes = g_malloc(stdFunctionals[SPW92].n*sizeof(MPQCFunctionalType));
	stdFunctionals[SPW92].listOfTypes[0]= SlaterX;
	stdFunctionals[SPW92].listOfTypes[1]= PW92LC;
	stdFunctionals[SPW92].coefficients = g_malloc(stdFunctionals[SPW92].n*sizeof(gdouble));
	stdFunctionals[SPW92].coefficients[0]= 1.0;
	stdFunctionals[SPW92].coefficients[1]= 1.0;

	stdFunctionals[BPW91].type = BPW91;
	stdFunctionals[BPW91].name = g_strdup("BPW91");
	stdFunctionals[BPW91].n = 3;
	stdFunctionals[BPW91].listOfTypes = g_malloc(stdFunctionals[BPW91].n*sizeof(MPQCFunctionalType));
	stdFunctionals[BPW91].listOfTypes[0]= SlaterX;
	stdFunctionals[BPW91].listOfTypes[1]= Becke88X;
	stdFunctionals[BPW91].listOfTypes[2]= PW91C;
	stdFunctionals[BPW91].coefficients = g_malloc(stdFunctionals[BPW91].n*sizeof(gdouble));
	stdFunctionals[BPW91].coefficients[0]= 1.0;
	stdFunctionals[BPW91].coefficients[1]= 1.0;
	stdFunctionals[BPW91].coefficients[2]= 1.0;

	stdFunctionals[BP86].type = BP86;
	stdFunctionals[BP86].name = g_strdup("BP86");
	stdFunctionals[BP86].n = 4;
	stdFunctionals[BP86].listOfTypes = g_malloc(stdFunctionals[BP86].n*sizeof(MPQCFunctionalType));
	stdFunctionals[BP86].listOfTypes[0]= SlaterX;
	stdFunctionals[BP86].listOfTypes[1]= Becke88X;
	stdFunctionals[BP86].listOfTypes[2]= P86C;
	stdFunctionals[BP86].listOfTypes[3]= PZ81LC;
	stdFunctionals[BP86].coefficients = g_malloc(stdFunctionals[BP86].n*sizeof(gdouble));
	stdFunctionals[BP86].coefficients[0]= 1.0;
	stdFunctionals[BP86].coefficients[1]= 1.0;
	stdFunctionals[BP86].coefficients[2]= 1.0;
	stdFunctionals[BP86].coefficients[3]= 1.0;

	stdFunctionals[B3LYP].type = B3LYP;
	stdFunctionals[B3LYP].name = g_strdup("B3LYP");
	stdFunctionals[B3LYP].n = 5;
	stdFunctionals[B3LYP].listOfTypes = g_malloc(stdFunctionals[B3LYP].n*sizeof(MPQCFunctionalType));
	stdFunctionals[B3LYP].listOfTypes[0]= HFX;
	stdFunctionals[B3LYP].listOfTypes[1]= SlaterX;
	stdFunctionals[B3LYP].listOfTypes[2]= Becke88X;
	stdFunctionals[B3LYP].listOfTypes[3]= VWN1LCRPA;
	stdFunctionals[B3LYP].listOfTypes[4]=  LYPC;
	stdFunctionals[B3LYP].coefficients = g_malloc(stdFunctionals[B3LYP].n*sizeof(gdouble));
	stdFunctionals[B3LYP].coefficients[0]= 0.2;
	stdFunctionals[B3LYP].coefficients[1]= 0.8;
	stdFunctionals[B3LYP].coefficients[2]= 0.72;
	stdFunctionals[B3LYP].coefficients[3]= 0.19;
	stdFunctionals[B3LYP].coefficients[4]= 0.81;

	stdFunctionals[B3PW91].type = B3PW91;
	stdFunctionals[B3PW91].name = g_strdup("B3PW91");
	stdFunctionals[B3PW91].n = 5;
	stdFunctionals[B3PW91].listOfTypes = g_malloc(stdFunctionals[B3PW91].n*sizeof(MPQCFunctionalType));
	stdFunctionals[B3PW91].listOfTypes[0]= HFX;
	stdFunctionals[B3PW91].listOfTypes[1]= SlaterX;
	stdFunctionals[B3PW91].listOfTypes[2]= Becke88X;
	stdFunctionals[B3PW91].listOfTypes[3]= PW91C;
	stdFunctionals[B3PW91].listOfTypes[4]= PW92LC;
	stdFunctionals[B3PW91].coefficients = g_malloc(stdFunctionals[B3PW91].n*sizeof(gdouble));
	stdFunctionals[B3PW91].coefficients[0]= 0.2;
	stdFunctionals[B3PW91].coefficients[1]= 0.8;
	stdFunctionals[B3PW91].coefficients[2]= 0.72;
	stdFunctionals[B3PW91].coefficients[3]= 0.19;
	stdFunctionals[B3PW91].coefficients[4]= 0.81;

	stdFunctionals[B3P86].type = B3P86;
	stdFunctionals[B3P86].name = g_strdup("B3P86");
	stdFunctionals[B3P86].n = 5;
	stdFunctionals[B3P86].listOfTypes = g_malloc(stdFunctionals[B3P86].n*sizeof(MPQCFunctionalType));
	stdFunctionals[B3P86].listOfTypes[0]= HFX;
	stdFunctionals[B3P86].listOfTypes[1]= SlaterX;
	stdFunctionals[B3P86].listOfTypes[2]= Becke88X;
	stdFunctionals[B3P86].listOfTypes[3]= P86C;
	stdFunctionals[B3P86].listOfTypes[4]= VWN1LCRPA;
	stdFunctionals[B3P86].coefficients = g_malloc(stdFunctionals[B3P86].n*sizeof(gdouble));
	stdFunctionals[B3P86].coefficients[0]= 0.2;
	stdFunctionals[B3P86].coefficients[1]= 0.8;
	stdFunctionals[B3P86].coefficients[2]= 0.72;
	stdFunctionals[B3P86].coefficients[3]= 0.19;
	stdFunctionals[B3P86].coefficients[4]= 0.81;

	stdFunctionals[PW91].type = PW91;
	stdFunctionals[PW91].name = g_strdup("PW91");
	stdFunctionals[PW91].n = 2;
	stdFunctionals[PW91].listOfTypes = g_malloc(stdFunctionals[PW91].n*sizeof(MPQCFunctionalType));
	stdFunctionals[PW91].listOfTypes[0]= PW91X;
	stdFunctionals[PW91].listOfTypes[1]= PW91C;
	stdFunctionals[PW91].coefficients = g_malloc(stdFunctionals[PW91].n*sizeof(gdouble));
	stdFunctionals[PW91].coefficients[0]= 1.0;
	stdFunctionals[PW91].coefficients[1]= 1.0;

	stdFunctionals[PBE].type = PBE;
	stdFunctionals[PBE].name = g_strdup("PBE");
	stdFunctionals[PBE].n = 2;
	stdFunctionals[PBE].listOfTypes = g_malloc(stdFunctionals[PBE].n*sizeof(MPQCFunctionalType));
	stdFunctionals[PBE].listOfTypes[0]= PBEX;
	stdFunctionals[PBE].listOfTypes[1]= PBEC;
	stdFunctionals[PBE].coefficients = g_malloc(stdFunctionals[PBE].n*sizeof(gdouble));
	stdFunctionals[PBE].coefficients[0]= 1.0;
	stdFunctionals[PBE].coefficients[1]= 1.0;


	stdFunctionals[mPW_PW91_PW91].type = mPW_PW91_PW91;
	stdFunctionals[mPW_PW91_PW91].name = g_strdup("mPW(PW91)PW91");
	stdFunctionals[mPW_PW91_PW91].n = 2;
	stdFunctionals[mPW_PW91_PW91].listOfTypes = g_malloc(stdFunctionals[mPW_PW91_PW91].n*sizeof(MPQCFunctionalType));
	stdFunctionals[mPW_PW91_PW91].listOfTypes[0]= mPW91_PW91X;
	stdFunctionals[mPW_PW91_PW91].listOfTypes[1]= PW91C;
	stdFunctionals[mPW_PW91_PW91].coefficients = g_malloc(stdFunctionals[mPW_PW91_PW91].n*sizeof(gdouble));
	stdFunctionals[mPW_PW91_PW91].coefficients[0]= 1.0;
	stdFunctionals[mPW_PW91_PW91].coefficients[1]= 1.0;

	stdFunctionals[mPWPW91].type = mPWPW91;
	stdFunctionals[mPWPW91].name = g_strdup("mPWPW91");
	stdFunctionals[mPWPW91].n = 2;
	stdFunctionals[mPWPW91].listOfTypes = g_malloc(stdFunctionals[mPWPW91].n*sizeof(MPQCFunctionalType));
	stdFunctionals[mPWPW91].listOfTypes[0]= mPW91_mPW91X;
	stdFunctionals[mPWPW91].listOfTypes[1]= PW91C;
	stdFunctionals[mPWPW91].coefficients = g_malloc(stdFunctionals[mPWPW91].n*sizeof(gdouble));
	stdFunctionals[mPWPW91].coefficients[0]= 1.0;
	stdFunctionals[mPWPW91].coefficients[1]= 1.0;

	stdFunctionals[mPW1PW91].type = mPW1PW91;
	stdFunctionals[mPW1PW91].name = g_strdup("mPW1PW91");
	stdFunctionals[mPW1PW91].n = 3;
	stdFunctionals[mPW1PW91].listOfTypes = g_malloc(stdFunctionals[mPW1PW91].n*sizeof(MPQCFunctionalType));
	stdFunctionals[mPW1PW91].listOfTypes[0]= HFX;
	stdFunctionals[mPW1PW91].listOfTypes[1]= mPW91_mPW91X;
	stdFunctionals[mPW1PW91].listOfTypes[2]= PW91C;
	stdFunctionals[mPW1PW91].coefficients = g_malloc(stdFunctionals[mPW1PW91].n*sizeof(gdouble));
	stdFunctionals[mPW1PW91].coefficients[0]= 0.16;
	stdFunctionals[mPW1PW91].coefficients[1]= 0.84;
	stdFunctionals[mPW1PW91].coefficients[2]= 1.0;
}
/************************************************************************************************************/
void freeMPQCStdFunctionals()
{
	gint n = mPW1PW91+1;
	gint i;
	if(!stdFunctionals)return;
	for(i=0;i<n;i++)
	{
		if(stdFunctionals[i].listOfTypes)g_free(stdFunctionals[i].listOfTypes);
		if(stdFunctionals[i].coefficients)g_free(stdFunctionals[i].coefficients);
	}
	g_free(stdFunctionals);
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
static void changedEntrySumDensityFunctional(GtkWidget *entry, gpointer data)
{
	G_CONST_RETURN gchar* entryText = NULL;
	gint* numFunctionals;
	gdouble* coefficients;
	 
	if(!GTK_IS_WIDGET(entry)) return;
	entryText = gtk_entry_get_text(GTK_ENTRY(entry));

	numFunctionals = g_object_get_data(G_OBJECT (entry), "Value");
	if(!numFunctionals)return;
	coefficients = g_object_get_data(G_OBJECT (entry), "Coefficients");
	if(!coefficients)return;
	if(strlen(entryText)<1)
	{
		if(*numFunctionals>=0) coefficients[*numFunctionals] = 0.0;
	}
	else
	{
		if(*numFunctionals>=0) coefficients[*numFunctionals] = atof(entryText);
	}
}
/***********************************************************************************************/
void createXCFunctionalsFrame(GtkWidget *box, gchar* title, gdouble* coefficients)
{
	GtkWidget* frame;
	GtkWidget* vboxFrame;
	GtkWidget* entry = NULL;
	GtkWidget *table = NULL;
	gint n = G96X + 1;
	gint i;
	gchar* t = NULL;

	table = gtk_table_new(5,4,FALSE);

	frame = gtk_frame_new (title);
	gtk_widget_show (frame);
	gtk_box_pack_start (GTK_BOX (box), frame, TRUE, TRUE, 3);
	gtk_frame_set_label_align (GTK_FRAME (frame), 0.5, 0.5);

	vboxFrame = gtk_vbox_new (FALSE, 3);
	gtk_widget_show (vboxFrame);
	gtk_container_add (GTK_CONTAINER (frame), vboxFrame);

	gtk_box_pack_start (GTK_BOX (vboxFrame), table, TRUE, TRUE, 0);

	t = g_malloc(BSIZE*sizeof(gchar));
	for(i=0;i<n;i++)
		if(
			(strstr(functionals[i].name,"X") && strstr(title,_("Exchange")))
		      || (strstr(functionals[i].name,"C") && strstr(title,_("Correlation")))
		  )
		{
			if(strstr(functionals[i].name,"LSDAC")) continue;
			if(!strstr(functionals[i].name,"HF"))
				if(strstr(functionals[i].name,")") || strstr(functionals[i].name,"RPA")) continue;
			add_label_table(table, functionals[i].name, (gushort)i, 0);
			add_label_table(table, ":", (gushort)i, 1);
			entry = gtk_entry_new();
			if(coefficients[i]!=0)
			{
				sprintf(t,"%0.3f",coefficients[i]);
				gtk_entry_set_text(GTK_ENTRY(entry),t);
			}
			add_widget_table(table, entry, (gushort)i, 2);
			g_object_set_data(G_OBJECT (entry), "Value",&functionals[i].type);
			g_object_set_data(G_OBJECT (entry), "Coefficients",coefficients);
			g_signal_connect(G_OBJECT(entry),"changed", G_CALLBACK(changedEntrySumDensityFunctional),NULL);
		}
	g_free(t);
}
/************************************************************************************************************/
static void destroyWindow(GtkWidget *win)
{
	gdouble* coefficients = g_object_get_data(G_OBJECT (win), "Coefficients");
	if(coefficients) g_free(coefficients);
	gtk_widget_destroy(win);
}
/************************************************************************************************************/
static void setSumDensityFunctionals(GtkWidget *win,gpointer data)
{
	gdouble* coefficients = g_object_get_data(G_OBJECT (win), "Coefficients");
	if(coefficients)
	{
		gint n = G96X+1;
		gint i;
		for(i=0;i<n;i++) sumFunctionals[i] = coefficients[i];
	}
}
/************************************************************************************************************/
void mpqcSumDensityFunctionalWindow()
{
	GtkWidget *button;
	GtkWidget *hbox = NULL;
	GtkWidget *win = NULL;
	GtkWidget *table = gtk_table_new(1,2,FALSE);
	gint n = G96X+1;
	gdouble* coefficients = NULL;
	gint i;

	coefficients = g_malloc(n*sizeof(gdouble));;
	for(i=0;i<n;i++)
		coefficients[i] = sumFunctionals[i];

	win= gtk_dialog_new ();
	gtk_window_set_position(GTK_WINDOW(win),GTK_WIN_POS_CENTER);
	gtk_window_set_transient_for(GTK_WINDOW(win),GTK_WINDOW(Fenetre));
	gtk_window_set_title(&GTK_DIALOG(win)->window,_("MPQC Sum Density functionals"));
    	gtk_window_set_modal (GTK_WINDOW (win), TRUE);
	g_object_set_data(G_OBJECT (win), "Coefficients", coefficients);

	init_child(win, destroyWindow,_(" MPQC Sum. Dens. "));
	g_signal_connect(G_OBJECT(win),"delete_event",(GCallback)destroy_children,NULL);

	gtk_widget_realize(win);

	gtk_box_pack_start (GTK_BOX( GTK_DIALOG(win)->vbox), table, FALSE, TRUE, 5);

	hbox =addHboxToTable(table, 0, 0, 1, 1);
	createXCFunctionalsFrame(hbox,_("Exchange functionals"), coefficients);

	hbox =addHboxToTable(table, 0, 1, 1, 1);
	createXCFunctionalsFrame(hbox,_("Correlation functionals"), coefficients);

	button = create_button(win,_("OK"));
	gtk_box_pack_start (GTK_BOX( GTK_DIALOG(win)->action_area), button, FALSE, TRUE, 5);
	GTK_WIDGET_SET_FLAGS(button, GTK_CAN_DEFAULT);
	gtk_widget_grab_default(button);
	gtk_widget_show (button);
	g_signal_connect_swapped(G_OBJECT(button), "clicked",G_CALLBACK(setSumDensityFunctionals),GTK_OBJECT(win));
	g_signal_connect_swapped(G_OBJECT(button), "clicked",G_CALLBACK(destroy_children),GTK_OBJECT(win));

	button = create_button(win,_("Close"));
  	gtk_box_pack_end (GTK_BOX( GTK_DIALOG(win)->action_area), button, FALSE, TRUE, 5);  
	g_signal_connect_swapped(G_OBJECT(button), "clicked", G_CALLBACK(destroy_children),GTK_OBJECT(win));
	GTK_WIDGET_SET_FLAGS(button, GTK_CAN_DEFAULT);
	gtk_widget_show (button);

	gtk_widget_show_all(win);
}
