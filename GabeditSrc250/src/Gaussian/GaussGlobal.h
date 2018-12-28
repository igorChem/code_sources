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

#ifndef __GABEDIT_GAUSSGLOBAL_H__
#define __GABEDIT_GAUSSGLOBAL_H__

#define NHBOXT_MAX 6
#define NHBOXM_MAX 6
#define NHBOXB_MAX 5
#define NMETHODS 3
#define NCHECKBUTTON 9
GtkWidget *TextTitle;
gchar *Tfreq;
gchar *Tscf;
gchar *Tpolar;
gchar* TtimeDependent;
gchar *Types;

GtkWidget* FrameT;
GtkWidget* VboxT;
guint nHboxT;
GtkWidget* HboxT[NHBOXT_MAX];
GtkWidget *EntryTypes[NHBOXT_MAX];

gchar *Methodes[NMETHODS];
gchar *Basis[NMETHODS];
GtkWidget* FrameM[NMETHODS];
GtkWidget* VboxM[NMETHODS];
guint nHboxM;
GtkWidget* HboxM[NHBOXM_MAX][NMETHODS];
GtkWidget *EntryMethods[NHBOXM_MAX][NMETHODS];
GtkWidget* FrameB[NMETHODS];
GtkWidget* VboxB[NMETHODS];
guint nHboxB;
GtkWidget* HboxB[NHBOXB_MAX][NMETHODS];
GtkWidget *EntryBasis[NHBOXB_MAX][NMETHODS];
GtkWidget *NoteBookMB;
gint NM;

GtkWidget* CheckButtons[NCHECKBUTTON];
GtkWidget *EntryCS[6];
gboolean GeomFromCheck;
gchar* StrLink;

#endif /* __GABEDIT_GAUSSGLOBAL_H__ */

