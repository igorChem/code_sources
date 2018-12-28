/* IRSpectrum.c */
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
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include "../Common/Global.h"
#include "../Utils/Constants.h"
#include "../Utils/UtilsInterface.h"
#include "../Utils/Utils.h"
#include "../Files/FileChooser.h"
#include "../Common/Windows.h"
#include "../Utils/GabeditXYPlot.h"
#include "../Display/Vibration.h"
#include "../Spectrum/SpectrumWin.h"
#include "../Spectrum/IGVPT2Spectrum.h"

/********************************************************************************/
static void check_microm_cmm1_toggled(GtkToggleButton *togglebutton, gpointer user_data)
{
	GtkWidget* xyplot = NULL;
	GList* data_list = NULL;
	GList* current = NULL;
	XYPlotWinData* data;
	gboolean microm = FALSE;
	GtkWidget* window = NULL;
	gdouble xmax = 0;
	gdouble ymax = 0;

	if(!user_data || !G_IS_OBJECT(user_data)) return;

	xyplot = GTK_WIDGET(user_data);
	data_list = g_object_get_data(G_OBJECT (xyplot), "DataList");

	if(!data_list) return;
	window = g_object_get_data(G_OBJECT (xyplot), "Window");

	microm = gtk_toggle_button_get_active(togglebutton);
	current=g_list_first(data_list);
	for(; current != NULL; current = current->next)
	{
		gint loop;
		data = (XYPlotWinData*)current->data;
		for (loop=0; loop<data->size; loop++)
		{
			if(data->x[loop]!=0)
			{
				if(microm) data->x[loop]= 10000.0/data->x[loop];
				else data->x[loop]= 10000.0/data->x[loop];
			}
			else
				data->x[loop]= 1.0e-10;
			if(xmax<data->x[loop]) xmax = data->x[loop];
			if(ymax<data->y[loop]) ymax = data->y[loop];
		}
	}
	xmax = xmax*1.5;
	ymax = ymax*2.0;
	spectrum_win_set_xmin(window, 0.0);
	spectrum_win_set_xmax(window, xmax);
	spectrum_win_set_ymax(window, ymax);
	spectrum_win_set_half_width(window, fabs(xmax/50));
	spectrum_win_reset_ymin_ymax(GTK_WIDGET(xyplot));
	if(microm)
	{
		spectrum_win_set_xlabel(window, "&#181;m");
	}
	else
		spectrum_win_set_xlabel(window, "cm<sup>-1</sup>");
	gtk_widget_queue_draw(GTK_WIDGET(xyplot));

}
/********************************************************************************/
static void reflectXY(GtkWidget *buttonReflect, gpointer user_data)
{
	GtkWidget* xyplot = NULL;
	GList* data_list = NULL;
	GList* current = NULL;
	XYPlotWinData* data;
	gboolean microm = FALSE;
	gdouble xmax = 0;
	gdouble ymax = 0;
	gboolean rx;
	gboolean ry;

	if(!user_data || !G_IS_OBJECT(user_data)) return;

	xyplot = GTK_WIDGET(user_data);
	data_list = g_object_get_data(G_OBJECT (xyplot), "DataList");

	if(!data_list) return;

	gabedit_xyplot_get_reflects (GABEDIT_XYPLOT (xyplot),&rx, &ry);

	gabedit_xyplot_reflect_x (GABEDIT_XYPLOT(xyplot), !rx);
        gabedit_xyplot_reflect_y (GABEDIT_XYPLOT(xyplot), !ry);

	gtk_widget_queue_draw(GTK_WIDGET(xyplot));

}
/********************************************************************************/
GtkWidget* createIRSpectrumWin(gint numberOfFrequencies, gdouble* frequencies, gdouble* intensities)
{
	GtkWidget* window = spectrum_win_new_with_xy(_("IR spectrum"),  numberOfFrequencies, frequencies, intensities);
	GtkWidget* hbox = g_object_get_data(G_OBJECT (window), "HBoxData");
	GtkWidget* hbox2 = g_object_get_data(G_OBJECT (window), "HBoxData2");
	GtkWidget* xyplot = g_object_get_data(G_OBJECT (window), "XYPLOT");
	GtkWidget* check_microm_cmm1 = NULL;
	GtkWidget* tmp_hbox = NULL;
	GtkWidget* buttonReflect = NULL;

	spectrum_win_relect_x(window, TRUE);
	spectrum_win_relect_y(window, TRUE);
	spectrum_win_set_half_width(window, 20.0);
	spectrum_win_set_xmin(window, 0.0);
	set_icone(window);

	if(!hbox) return window;
	if(!hbox2) return window;

	tmp_hbox=gtk_hbox_new(FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox), tmp_hbox, FALSE, FALSE, 30);
	gtk_widget_show(tmp_hbox);

	check_microm_cmm1 = gtk_check_button_new_with_label("microm<=>cm-1");
	gtk_box_pack_start(GTK_BOX(tmp_hbox), check_microm_cmm1, FALSE, FALSE, 4);
	gtk_widget_show(check_microm_cmm1);

	g_signal_connect(G_OBJECT(check_microm_cmm1), "toggled", G_CALLBACK(check_microm_cmm1_toggled), xyplot);
	spectrum_win_set_xlabel(window, "cm<sup>-1</sup>");

	tmp_hbox=gtk_hbox_new(FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox2), tmp_hbox, FALSE, FALSE, 30);
	gtk_widget_show(tmp_hbox);

	buttonReflect = create_button(window,_(" Reflect "));
	gtk_box_pack_start(GTK_BOX(tmp_hbox), buttonReflect, FALSE, FALSE, 4);
        g_signal_connect(G_OBJECT(buttonReflect), "clicked", G_CALLBACK(reflectXY), xyplot);
	gtk_widget_show(buttonReflect);


	return window;
}
/********************************************************************************/
static void messageErrorFreq(gchar* fileName)
{
	gchar buffer[BSIZE];
	sprintf(buffer,_("Sorry, I can not read frequencies from '%s' file\n"),fileName);
  	Message(buffer,_("Error"),TRUE);
}
/********************************************************************************/
static void messageErrorInt(gchar* fileName)
{
	gchar buffer[BSIZE];
	sprintf(buffer,_("Sorry, I can not read frequencies from '%s' file\n"),fileName);
  	Message(buffer,_("Error"),TRUE);
}
/********************************************************************************/
static gboolean read_gabedit_molden_file(GabeditFileChooser *SelecFile, gint response_id)
{
	gchar *FileName;
 	gchar t[BSIZE];
 	gboolean OK;
 	FILE *fd;
	gint i;
	gint ne;
	gint numberOfFrequencies = 0;
	gdouble* frequencies = NULL;
	gdouble* intensities = NULL;
	gdouble a;

	if(response_id != GTK_RESPONSE_OK) return FALSE;
 	FileName = gabedit_file_chooser_get_current_file(SelecFile);

 	fd = FOpen(FileName, "rb");
 	OK=FALSE;

  	while(!feof(fd))
  	{
    		if(!fgets(t,BSIZE,fd)) break;
		if(strstr(t,"[FREQ"))
		{
			OK = TRUE;
			break;
		}
	}
	if(!OK)
	{
		messageErrorFreq(FileName);
		if(fd) fclose(fd);
		return FALSE;
	}
  	while(!feof(fd))
  	{
    		if(!fgets(t,BSIZE,fd)) break;
		if(strstr(t,"[")) break;
		if(this_is_a_backspace(t)) break;
		ne = sscanf(t,"%lf",&a);
		if(ne != 1)
		{
			messageErrorFreq(FileName);
			if(frequencies) g_free(frequencies);
			if(intensities) g_free(intensities);
			fclose(fd);
			return FALSE;
		}
		numberOfFrequencies++;
		frequencies = g_realloc(frequencies, numberOfFrequencies*sizeof(gdouble));
		intensities = g_realloc(intensities, numberOfFrequencies*sizeof(gdouble));
		frequencies[numberOfFrequencies-1] = a;
		intensities[numberOfFrequencies-1] = 0;
	}
	if(numberOfFrequencies<1)
	{
		messageErrorFreq(FileName);
		if(frequencies) g_free(frequencies);
		if(intensities) g_free(intensities);
		fclose(fd);
		return FALSE;
	}
	OK = FALSE;
	if(strstr(t,"[INT"))OK = TRUE;
  	while(!feof(fd) && !OK)
  	{
    		if(!fgets(t,BSIZE,fd)) break;
		if(strstr(t,"[INT"))
		{
			OK = TRUE;
			break;
		}
	}
	if(!OK)
	{
		messageErrorInt(FileName);
		if(fd) fclose(fd);
		return FALSE;
	}
	for(i=0;i<numberOfFrequencies;i++)
	{
    		if(!fgets(t,BSIZE,fd))
		{
			messageErrorInt(FileName);
			if(frequencies) g_free(frequencies);
			if(intensities) g_free(intensities);
			fclose(fd);
			return FALSE;
		}
		ne = sscanf(t,"%lf",&a);
		if(ne != 1)
		{
			messageErrorInt(FileName);
			if(frequencies) g_free(frequencies);
			if(intensities) g_free(intensities);
			fclose(fd);
			return FALSE;
		}
		intensities[i] = a;
	}

	createIRSpectrumWin(numberOfFrequencies, frequencies, intensities);



	if(frequencies) g_free(frequencies);
	if(intensities) g_free(intensities);
	fclose(fd);
	return TRUE;

}
/********************************************************************************/
static void read_gabedit_file_dlg()
{
	GtkWidget* filesel = 
 	file_chooser_open(read_gabedit_molden_file,
			_("Read frequencies and intensities from a Gabedit file"),
			GABEDIT_TYPEFILE_GABEDIT,GABEDIT_TYPEWIN_OTHER);

	gtk_window_set_modal (GTK_WINDOW (filesel), TRUE);
}
/********************************************************************************/
static void read_molden_file_dlg()
{
	GtkWidget* filesel = 
 	file_chooser_open(read_gabedit_molden_file,
			_("Read frequencies and intensities from a Molden file"),
			GABEDIT_TYPEFILE_MOLDEN,GABEDIT_TYPEWIN_OTHER);

	gtk_window_set_modal (GTK_WINDOW (filesel), TRUE);
}
/********************************************************************************/
static gboolean read_mpqc_file(GabeditFileChooser *SelecFile, gint response_id)
{
	return FALSE;
}
/********************************************************************************/
static void read_mpqc_file_dlg()
{
	GtkWidget* filesel = 
 	file_chooser_open(read_mpqc_file,
			_("Read frequencies and intensities from a MPQC output file"),
			GABEDIT_TYPEFILE_MPQC,GABEDIT_TYPEWIN_OTHER);

	gtk_window_set_modal (GTK_WINDOW (filesel), TRUE);
}
/********************************************************************************/
static gboolean read_molpro_file(GabeditFileChooser *SelecFile, gint response_id)
{
 	gchar t[BSIZE];
 	gchar sdum1[BSIZE];
 	gchar sdum2[BSIZE];
 	gboolean OK;
 	guint taille=BSIZE;
	gint k;
	gint nf;
	gdouble freq[5];
	gdouble IRIntensity[5];
	gint numberOfFrequencies = 0;
	gdouble* frequencies = NULL;
	gdouble* intensities = NULL;

	gchar *FileName;
 	FILE *fd;

	if(response_id != GTK_RESPONSE_OK) return FALSE;
 	FileName = gabedit_file_chooser_get_current_file(SelecFile);

 	fd = FOpen(FileName, "rb");
	if(!fd)
	{
		messageErrorFreq(FileName);
		if(frequencies) g_free(frequencies);
		if(intensities) g_free(intensities);
		return FALSE;
	}

 	OK=FALSE;
 	while(!feof(fd))
	{
    		if(!feof(fd)) { char* e = fgets(t,taille,fd);}
	 	if ( strstr( t,"Normal Modes") )
	  	{
			OK = TRUE;
			break;
	  	}
	}

	if(!OK)
	{
		messageErrorFreq(FileName);
		if(frequencies) g_free(frequencies);
		if(intensities) g_free(intensities);
		return FALSE;
	}

	numberOfFrequencies = 0;
  	while(!feof(fd))
  	{
    		if(!fgets(t,taille,fd)) break;
	 	if(strstr( t,"Normal Modes of imaginary frequencies") ) break;
	 	if(strstr( t,"Normal Modes of low/zero frequencies") ) break;
		if(!strstr(t,"Wavenumbers")) continue;
		nf = sscanf(t,"%s %s %lf %lf %lf %lf %lf", sdum1,sdum2, &freq[0],&freq[1],&freq[2],&freq[3],&freq[4]);
		nf -= 2;
    		if(!fgets(t,taille,fd)) break;
		sscanf(t,"%s %s %lf %lf %lf %lf %lf",
				sdum1,sdum2,
				&IRIntensity[0],&IRIntensity[1],&IRIntensity[2],&IRIntensity[3],&IRIntensity[4]);
		for(k=0;k<nf;k++)
		{
			numberOfFrequencies++;
			frequencies = g_realloc(frequencies, numberOfFrequencies*sizeof(gdouble));
			intensities = g_realloc(intensities, numberOfFrequencies*sizeof(gdouble));
			frequencies[numberOfFrequencies-1] = freq[k];
			intensities[numberOfFrequencies-1] = IRIntensity[k];
		}
	}
	if(numberOfFrequencies>0)
	{
		createIRSpectrumWin(numberOfFrequencies, frequencies, intensities);
	}
	else
	{
		messageErrorFreq(FileName);
		if(frequencies) g_free(frequencies);
		if(intensities) g_free(intensities);
		return FALSE;
	}

	if(frequencies) g_free(frequencies);
	if(intensities) g_free(intensities);
	return TRUE;
}
/********************************************************************************/
static void read_molpro_file_dlg()
{
	GtkWidget* filesel = 
 	file_chooser_open(read_molpro_file,
			_("Read frequencies and intensities from a Molpro output file"),
			GABEDIT_TYPEFILE_MOLPRO,GABEDIT_TYPEWIN_OTHER);

	gtk_window_set_modal (GTK_WINDOW (filesel), TRUE);
}
/********************************************************************************/
static gint read_dalton_modes_MOLHES(FILE* fd, gchar *FileName)
{
 	gchar t[BSIZE];
 	gchar sdum1[100];
 	gchar sdum2[100];
 	gboolean OK;
	gint nf;
	gdouble freq;
	gint numMode;
	gchar sym[50];
	gdouble IR = 0;
	gint numberOfFrequencies = 0;
	gdouble* frequencies = NULL;
	gdouble* intensities = NULL;

 	OK=FALSE;
 	while(!feof(fd))
	{
		if(!fgets(t,BSIZE,fd))break;
	 	if ( strstr( t,"Vibrational Frequencies and IR Intensities") )
	  	{
			OK = TRUE;
			break;
	  	}
	}

	if(!OK) 
	{
		messageErrorFreq(FileName);
		if(frequencies) g_free(frequencies);
		if(intensities) g_free(intensities);
		return 1;
	}

 	OK=FALSE;
 	while(!feof(fd))
	{
		if(!fgets(t,BSIZE,fd))break;
	 	if ( strstr( t,"cm-1") && strstr( t,"hartrees")  )
	  	{
			if(!fgets(t,BSIZE,fd))break;
			OK = TRUE;
			break;
	  	}
	}
	if(!OK)
	{
		messageErrorFreq(FileName);
		if(frequencies) g_free(frequencies);
		if(intensities) g_free(intensities);
		return 1;
	}

  	while(!feof(fd))
  	{
		if(!fgets(t,BSIZE,fd))
		{
			messageErrorFreq(FileName);
			if(frequencies) g_free(frequencies);
			if(intensities) g_free(intensities);
			return 2;
		}
		if(this_is_a_backspace(t)) continue;
		if(strstr(t,"Normal Coordinates")) break;

		nf = sscanf(t,"%d %s %s %s %s", &numMode, sym, sdum1,sdum2, sdum2);
		if(nf<3)
		{
			messageErrorFreq(FileName);
			if(frequencies) g_free(frequencies);
			if(intensities) g_free(intensities);
			return 2;
		}
		if(strstr(sdum1,"i")) freq = -atof(sdum1);
		else freq = atof(sdum1);

		if(nf==5) IR = atof(sdum2);
		else IR = 0.0;
		numberOfFrequencies++;
		frequencies = g_realloc(frequencies, numberOfFrequencies*sizeof(gdouble));
		intensities = g_realloc(intensities, numberOfFrequencies*sizeof(gdouble));

		frequencies[numberOfFrequencies-1] = freq;
		intensities[numberOfFrequencies-1] = IR;
	}
	if(numberOfFrequencies>0)
	{
		createIRSpectrumWin(numberOfFrequencies, frequencies, intensities);
	}
	else
	{
		messageErrorFreq(FileName);
		if(frequencies) g_free(frequencies);
		if(intensities) g_free(intensities);
	}

	if(frequencies) g_free(frequencies);
	if(intensities) g_free(intensities);
	return 0;
}
/********************************************************************************/
static gboolean read_dalton_file(GabeditFileChooser *SelecFile, gint response_id)
{
	gchar *FileName;
 	FILE *fd;

	if(response_id != GTK_RESPONSE_OK) return FALSE;
 	FileName = gabedit_file_chooser_get_current_file(SelecFile);

 	fd = FOpen(FileName, "rb");
	read_dalton_modes_MOLHES(fd, FileName);
	fclose(fd);
	return TRUE;
}
/********************************************************************************/
static void read_dalton_file_dlg()
{
	GtkWidget* filesel = 
 	file_chooser_open(read_dalton_file,
			_("Read last frequencies and intensities from a Dalton output file"),
			GABEDIT_TYPEFILE_DALTON,GABEDIT_TYPEWIN_OTHER);

	gtk_window_set_modal (GTK_WINDOW (filesel), TRUE);
}
/********************************************************************************/
static gboolean read_gamess_file(GabeditFileChooser *SelecFile, gint response_id)
{
 	gchar t[BSIZE];
 	gboolean OK;
	gint i;
	gint nf;
	gint nir;
	gint nfMax = 5;
	gdouble freq[5];
	gdouble ir[5];
 	gchar* sdum[5*2];
 	gchar* tmp;
	gint numberOfFrequencies = 0;
	gdouble* frequencies = NULL;
	gdouble* intensities = NULL;
	gchar *FileName;
 	FILE *fd;

	if(response_id != GTK_RESPONSE_OK) return FALSE;
 	FileName = gabedit_file_chooser_get_current_file(SelecFile);

 	fd = FOpen(FileName, "rb");

 	OK=FALSE;
 	while(!feof(fd))
	{
		if(!fgets(t,BSIZE,fd))break;
	 	if ( strstr( t,"FREQUENCY:") )
	  	{
			OK = TRUE;
			break;
	  	}
	}

	if(!OK) 
	{
		messageErrorFreq(FileName);
		if(frequencies) g_free(frequencies);
		if(intensities) g_free(intensities);
		return FALSE;
	}
	for(i=0;i<nfMax*2;i++) sdum[i] = g_malloc(BSIZE*sizeof(gchar));

  	while(!feof(fd))
  	{
		gint nfi=0;
		if(!strstr( t,"FREQUENCY:"))
		{
			if(!fgets(t,BSIZE,fd)) break;
			continue;
		}

		tmp = strstr(t,":")+1;
		for(i=0;i<nfMax*2;i++) sprintf(sdum[i]," ");
		nfi = sscanf(tmp,"%s %s %s %s %s %s %s %s %s %s", sdum[0],sdum[1],sdum[2],sdum[3],sdum[4],
				sdum[5],sdum[6],sdum[7],sdum[8],sdum[9]
				);
		if(nfi<1)
		{
			for(i=0;i<nfMax*2;i++) g_free(sdum[i]);
			messageErrorFreq(FileName);
			if(frequencies) g_free(frequencies);
			if(intensities) g_free(intensities);
			return FALSE;
		}
		nf = 0;
		for(i=0;i<nfi;)
		{
			if(strstr(sdum[i+1],"I"))
			{
				freq[nf] = -atof(sdum[i]);
				i+=2;
			}
			else
			{
				freq[nf] = atof(sdum[i]);
				i+=1;
			}
			nf++;
		}
		nir=-1;
		while(fgets(t,BSIZE,fd) && strstr(t,":")) /* REDUCED MASS: IR INTENSITY: Raman, Depol,... backspace */
		{
			if(strstr(t,"IR INTENSITY:"))
			{
				tmp =  strstr(t,":")+1;
				nir = sscanf(tmp,"%s %s %s %s %s", sdum[0],sdum[1],sdum[2],sdum[3],sdum[4]);
			}
		}
		if(nf!=nir)
		{
			for(i=0;i<nfMax*2;i++) g_free(sdum[i]);
			messageErrorFreq(FileName);
			if(frequencies) g_free(frequencies);
			if(intensities) g_free(intensities);
			return FALSE;
		}
		for(i=0;i<nf;i++) ir[i] = atof(sdum[i]);

		for(i=0;i<nf;i++)
		{
			numberOfFrequencies++;
			frequencies = g_realloc(frequencies, numberOfFrequencies*sizeof(gdouble));
			intensities = g_realloc(intensities, numberOfFrequencies*sizeof(gdouble));
			frequencies[numberOfFrequencies-1] = freq[i];
			intensities[numberOfFrequencies-1] = ir[i];
		}
		if(!fgets(t,BSIZE,fd)) break;
	}
	if(numberOfFrequencies>0)
	{
		createIRSpectrumWin(numberOfFrequencies, frequencies, intensities);
	}
	else
	{
		messageErrorFreq(FileName);
	}


	if(frequencies) g_free(frequencies);
	if(intensities) g_free(intensities);
	fclose(fd);
	return TRUE;
}
/********************************************************************************/
static void read_gamess_file_dlg()
{
	GtkWidget* filesel = 
 	file_chooser_open(read_gamess_file,
			_("Read last frequencies and intensities from a Gamess output file"),
			GABEDIT_TYPEFILE_GAMESS,GABEDIT_TYPEWIN_OTHER);

	gtk_window_set_modal (GTK_WINDOW (filesel), TRUE);
}
/********************************************************************************/
static gboolean read_gamess_anharmonic_file(GabeditFileChooser *SelecFile, gint response_id)
{
 	gchar t[BSIZE];
 	gchar sdum1[BSIZE];
 	gchar sdum2[BSIZE];
 	gchar sdum3[BSIZE];
	gint nf;
	gint i;
	gdouble freq[3] = {0,0,0};
	gdouble IRIntensity[3] = {0,0,0};
	gint numberOfFrequencies = 0;
	gdouble* frequencies = NULL;
	gdouble* intensities = NULL;
	gchar *FileName;
 	FILE *fd;
	gchar** allreals = NULL;

	if(response_id != GTK_RESPONSE_OK) return FALSE;
 	FileName = gabedit_file_chooser_get_current_file(SelecFile);

 	fd = FOpen(FileName, "rb");
	if(!fd) return FALSE;

 	do 
 	{
 		while(!feof(fd))
		{
    			if(!feof(fd)) { char* e = fgets(t,BSIZE,fd);}
	 		/* if ( strstr( t,"reduced masses") )*/
	 		if ( strstr( t,"IR INTENSITIES ARE CALCULATED USING DIPOLE MOMENTS") )
	  		{
    				if(!feof(fd)) { char* e = fgets(t,BSIZE,fd);}/* MODE   FREQUENCY, CM-1  INTENSITY, KM/MOL */
				numberOfFrequencies = 0;
				break;
	  		}
		}
  		while(!feof(fd) )
  		{
			gint k;
    			if(!feof(fd)) { char* e = fgets(t,BSIZE,fd);}
			if(strstr(t,"FINISHED")) break;
			if(this_is_a_backspace(t)) break;
 			allreals =gab_split (t);
			k = 0;
			while(allreals && allreals[k]) k++;
			if(k==3)
			{
				numberOfFrequencies++;
				frequencies = g_realloc(frequencies, numberOfFrequencies*sizeof(gdouble));
				intensities = g_realloc(intensities, numberOfFrequencies*sizeof(gdouble));
				frequencies[numberOfFrequencies-1] = atof(allreals[1]);
				intensities[numberOfFrequencies-1] = atof(allreals[2]);
			}
			g_strfreev(allreals);
			allreals = NULL;
		}
 	}while(!feof(fd));

	if(numberOfFrequencies>0)
	{
		createIRSpectrumWin(numberOfFrequencies, frequencies, intensities);
	}
	else
	{
		messageErrorFreq(FileName);
	}


	if(frequencies) g_free(frequencies);
	if(intensities) g_free(intensities);
	fclose(fd);
	return TRUE;
}
/********************************************************************************/
static void read_gamess_anharmonic_file_dlg()
{
	GtkWidget* filesel = 
 	file_chooser_open(read_gamess_anharmonic_file,
			_("Read last Anharmonic Infrared spectrum from a GAMESS output file"),
			GABEDIT_TYPEFILE_GAUSSIAN,GABEDIT_TYPEWIN_OTHER);

	gtk_window_set_modal (GTK_WINDOW (filesel), TRUE);
}
/********************************************************************************/
static gboolean read_gaussian_file(GabeditFileChooser *SelecFile, gint response_id)
{
 	gchar t[BSIZE];
 	gchar sdum1[BSIZE];
 	gchar sdum2[BSIZE];
 	gchar sdum3[BSIZE];
	gint nf;
	gint i;
	gdouble freq[3] = {0,0,0};
	gdouble IRIntensity[3] = {0,0,0};
	gint numberOfFrequencies = 0;
	gdouble* frequencies = NULL;
	gdouble* intensities = NULL;
	gchar *FileName;
 	FILE *fd;

	if(response_id != GTK_RESPONSE_OK) return FALSE;
 	FileName = gabedit_file_chooser_get_current_file(SelecFile);

 	fd = FOpen(FileName, "rb");
	if(!fd) return FALSE;

 	do 
 	{
 		while(!feof(fd))
		{
    			if(!feof(fd)) { char* e = fgets(t,BSIZE,fd);}
	 		/* if ( strstr( t,"reduced masses") )*/
	 		if ( strstr( t,"and normal coordinates:") )
	  		{
				numberOfFrequencies = 0;
				break;
	  		}
		}
  		while(!feof(fd) )
  		{
    			if(!feof(fd)) { char* e = fgets(t,BSIZE,fd);}
			if(!strstr(t,"Frequencies --"))continue;
			if(this_is_a_backspace(t)) break;
			nf = sscanf(t,"%s %s %lf %lf %lf", sdum1,sdum2, &freq[0],&freq[1],&freq[2]);
			nf -=2;
			if(nf<=0 || nf>3) break;
			while(!feof(fd))
			{
    				if(!feof(fd)) { char* e = fgets(t,BSIZE,fd);}
				if(strstr(t,"IR Inten"))
				{
					sscanf(t,"%s %s %s %lf %lf %lf", sdum1,sdum2, sdum3, &IRIntensity[0],&IRIntensity[1],&IRIntensity[2]);
					break;
				}
			}
			for(i=0;i<nf;i++)
			{
				numberOfFrequencies++;
				frequencies = g_realloc(frequencies, numberOfFrequencies*sizeof(gdouble));
				intensities = g_realloc(intensities, numberOfFrequencies*sizeof(gdouble));
				frequencies[numberOfFrequencies-1] = freq[i];
				intensities[numberOfFrequencies-1] = IRIntensity[i];
			}
		}
 	}while(!feof(fd));

	if(numberOfFrequencies>0)
	{
		createIRSpectrumWin(numberOfFrequencies, frequencies, intensities);
	}
	else
	{
		messageErrorFreq(FileName);
	}


	if(frequencies) g_free(frequencies);
	if(intensities) g_free(intensities);
	fclose(fd);
	return TRUE;
}
/********************************************************************************/
static void read_gaussian_file_dlg()
{
	GtkWidget* filesel = 
 	file_chooser_open(read_gaussian_file,
			_("Read last frequencies and intensities from a Gaussian output file"),
			GABEDIT_TYPEFILE_GAUSSIAN,GABEDIT_TYPEWIN_OTHER);

	gtk_window_set_modal (GTK_WINDOW (filesel), TRUE);
}
/********************************************************************************/
static gboolean read_gaussian_anharmonic_file(GabeditFileChooser *SelecFile, gint response_id)
{
 	gchar t[BSIZE];
 	gchar sdum1[BSIZE];
 	gchar sdum2[BSIZE];
 	gchar sdum3[BSIZE];
	gint nf;
	gint i;
	gdouble freq[3] = {0,0,0};
	gdouble IRIntensity[3] = {0,0,0};
	gint numberOfFrequencies = 0;
	gdouble* frequencies = NULL;
	gdouble* intensities = NULL;
	gchar *FileName;
 	FILE *fd;
	gchar** allreals = NULL;

	if(response_id != GTK_RESPONSE_OK) return FALSE;
 	FileName = gabedit_file_chooser_get_current_file(SelecFile);

 	fd = FOpen(FileName, "rb");
	if(!fd) return FALSE;

 	do 
 	{
 		while(!feof(fd))
		{
    			if(!feof(fd)) { char* e = fgets(t,BSIZE,fd);}
	 		/* if ( strstr( t,"reduced masses") )*/
	 		if ( strstr( t,"Anharmonic Infrared Spectroscopy") )
	  		{
				numberOfFrequencies = 0;
				break;
	  		}
		}
  		while(!feof(fd) )
  		{
			gboolean fundamental = FALSE;
			gboolean overtones = FALSE;
			gboolean combination = FALSE;
 			while(!feof(fd))
			{
    				if(!feof(fd)) { char* e = fgets(t,BSIZE,fd);}
	 			/* if ( strstr( t,"reduced masses") )*/
	 			if ( strstr( t,"Fundamental Bands") )
	  			{
					fundamental = TRUE;
					overtones = FALSE;
					combination = FALSE;
    					if(!feof(fd)) { char* e = fgets(t,BSIZE,fd);}/* ----- */
    					if(!feof(fd)) { char* e = fgets(t,BSIZE,fd);} /* Mode(Quanta) */
					break;
	  			}
	 			if ( strstr( t,"Overtones") )
	  			{
					fundamental = FALSE;
					overtones = TRUE;
					combination = FALSE;
    					if(!feof(fd)) { char* e = fgets(t,BSIZE,fd);}
    					if(!feof(fd)) { char* e = fgets(t,BSIZE,fd);}
					break;
	  			}
	 			if ( strstr( t,"Combination Bands") )
	  			{
					fundamental = FALSE;
					overtones = FALSE;
					combination = TRUE;
    					if(!feof(fd)) { char* e = fgets(t,BSIZE,fd);}
    					if(!feof(fd)) { char* e = fgets(t,BSIZE,fd);}
					break;
	  			}
			}

 			while(!feof(fd))
			{
				gint k;
    				if(!feof(fd)) { char* e = fgets(t,BSIZE,fd);}
				if(strstr(t,"Leave")) break;
				if(this_is_a_backspace(t)) break;
 				allreals =gab_split (t);
				k = 0;
				while(allreals && allreals[k]) k++;
				if(k==5 && fundamental)
				{
					numberOfFrequencies++;
					frequencies = g_realloc(frequencies, numberOfFrequencies*sizeof(gdouble));
					intensities = g_realloc(intensities, numberOfFrequencies*sizeof(gdouble));
					frequencies[numberOfFrequencies-1] = atof(allreals[2]);
					intensities[numberOfFrequencies-1] = atof(allreals[4]);
				}
				if(k==4 && overtones)
				{
					numberOfFrequencies++;
					frequencies = g_realloc(frequencies, numberOfFrequencies*sizeof(gdouble));
					intensities = g_realloc(intensities, numberOfFrequencies*sizeof(gdouble));
					frequencies[numberOfFrequencies-1] = atof(allreals[2]);
					intensities[numberOfFrequencies-1] = atof(allreals[3]);
				}
				if(k==5 && combination)
				{
					numberOfFrequencies++;
					frequencies = g_realloc(frequencies, numberOfFrequencies*sizeof(gdouble));
					intensities = g_realloc(intensities, numberOfFrequencies*sizeof(gdouble));
					frequencies[numberOfFrequencies-1] = atof(allreals[3]);
					intensities[numberOfFrequencies-1] = atof(allreals[4]);
				}
				g_strfreev(allreals);
				allreals = NULL;
			}
			if(strstr(t,"Leave")) break;
			if(combination) break;
		}
 	}while(!feof(fd));

	if(numberOfFrequencies>0)
	{
		createIRSpectrumWin(numberOfFrequencies, frequencies, intensities);
	}
	else
	{
		messageErrorFreq(FileName);
	}


	if(frequencies) g_free(frequencies);
	if(intensities) g_free(intensities);
	fclose(fd);
	return TRUE;
}
/********************************************************************************/
static void read_gaussian_anharmonic_file_dlg()
{
	GtkWidget* filesel = 
 	file_chooser_open(read_gaussian_anharmonic_file,
			_("Read last Anharmonic Infrared spectrum from a Gaussian output file"),
			GABEDIT_TYPEFILE_GAUSSIAN,GABEDIT_TYPEWIN_OTHER);

	gtk_window_set_modal (GTK_WINDOW (filesel), TRUE);
}
/********************************************************************************/
static gboolean read_qchem_file(GabeditFileChooser *SelecFile, gint response_id)
{

	gchar *FileName;
	gint i;
 	gchar t[BSIZE];
 	gchar sdum1[BSIZE];
 	gchar sdum2[BSIZE];
 	gboolean OK;
 	FILE *fd;
 	guint taille=BSIZE;
	gint idum;
	gint nf;
	gdouble freq[3] = {0,0,0};
	gdouble IRIntensity[3] = {0,0,0};
	gint numberOfFrequencies = 0;
	gdouble *frequencies = NULL;
	gdouble *intensities = NULL;

	if(response_id != GTK_RESPONSE_OK) return FALSE;
 	FileName = gabedit_file_chooser_get_current_file(SelecFile);

 	fd = FOpen(FileName, "rb");
	if(!fd) return FALSE;

 	do 
 	{
 		OK=FALSE;
 		while(!feof(fd))
		{
    			if(!feof(fd)) { char* e = fgets(t,BSIZE,fd);}
	 		if (strstr( t,"VIBRATIONAL ANALYSIS") ) OK = TRUE;
	 		if (strstr( t,"Mode:") && OK ){ OK = TRUE; break;}
		}
		if(!OK) break;
  		while(!feof(fd) )
  		{
			if(!strstr(t,"Mode:")) break;
			nf = sscanf(t,"%s %d %d %d",sdum1,&idum,&idum,&idum);
			nf--;
			if(nf<0 || nf>3) break;

			if(!fgets(t,taille,fd)) break;
			sscanf(t,"%s %lf %lf %lf", sdum1, &freq[0],&freq[1],&freq[2]);
			while(!feof(fd))
			{
    				if(!feof(fd)) { char* e = fgets(t,BSIZE,fd);}
				if(strstr(t,"IR Intens:"))
				{
					sscanf(t,"%s %s %lf %lf %lf", sdum1,sdum2, &IRIntensity[0],&IRIntensity[1],&IRIntensity[2]);
					break;
				}
			}
			for(i=0;i<nf;i++)
			{
				numberOfFrequencies++;
				frequencies = g_realloc(frequencies, numberOfFrequencies*sizeof(gdouble));
				intensities = g_realloc(intensities, numberOfFrequencies*sizeof(gdouble));
				frequencies[numberOfFrequencies-1] = freq[i];
				intensities[numberOfFrequencies-1] = IRIntensity[i];
			}
			if(!strstr(t,"X      Y      Z"))
			while(!feof(fd))
			{
    				if(!feof(fd)) { char* e = fgets(t,BSIZE,fd);}
				if(strstr(t,"Mode:")) break; /* Mode: or END */
			}
		}
 	}while(!feof(fd));

	if(numberOfFrequencies>0)
	{
		createIRSpectrumWin(numberOfFrequencies, frequencies, intensities);
	}
	else
	{
		messageErrorFreq(FileName);
	}


	if(frequencies) g_free(frequencies);
	if(intensities) g_free(intensities);
	fclose(fd);

	return TRUE;
}
/********************************************************************************/
static void read_qchem_file_dlg()
{
	GtkWidget* filesel = 
 	file_chooser_open(read_qchem_file,
			_("Read last frequencies and intensities from a Q-Chem output file"),
			GABEDIT_TYPEFILE_QCHEM,GABEDIT_TYPEWIN_OTHER);

	gtk_window_set_modal (GTK_WINDOW (filesel), TRUE);
}
/********************************************************************************/
static gboolean read_nwchem_file(GabeditFileChooser *SelecFile, gint response_id)
{
	gchar *FileName;
 	gchar t[BSIZE];
 	gchar sdum1[BSIZE];
 	gboolean OK;
 	FILE *fd;
 	guint taille=BSIZE;
	gint numberOfFrequencies = 0;
	gdouble *frequencies = NULL;
	gdouble *intensities = NULL;

	if(response_id != GTK_RESPONSE_OK) return FALSE;
 	FileName = gabedit_file_chooser_get_current_file(SelecFile);

 	fd = FOpen(FileName, "rb");
	if(!fd) return FALSE;

 	do 
 	{
 		OK=FALSE;
		while(!feof(fd) )
                {
                        if(!fgets(t,taille,fd)) break;
                        if(strstr(t,"Projected Infra Red Intensities"))
                        {
				numberOfFrequencies = 0;
				if(frequencies) g_free(frequencies);
				if(intensities) g_free(intensities);
				frequencies = NULL;
				intensities = NULL;
                                if(!fgets(t,taille,fd)) break;
                                if(!fgets(t,taille,fd)) break;
                                 while(!feof(fd) )
                                {
                                        if(!fgets(t,taille,fd)) break;
					if(strstr(t,"---------"))break;
					numberOfFrequencies++;
					frequencies = g_realloc(frequencies, numberOfFrequencies*sizeof(gdouble));
					intensities = g_realloc(intensities, numberOfFrequencies*sizeof(gdouble));
                                        sscanf(t,"%s %lf %s %s %s %lf", sdum1,&frequencies[numberOfFrequencies-1],sdum1,sdum1,sdum1, &intensities[numberOfFrequencies-1]);
                                }
				OK = TRUE;
                        }
                }
		if(!OK) break;
 	}while(!feof(fd));

	if(numberOfFrequencies>0)
	{
		createIRSpectrumWin(numberOfFrequencies, frequencies, intensities);
	}
	else
	{
		messageErrorFreq(FileName);
	}


	if(frequencies) g_free(frequencies);
	if(intensities) g_free(intensities);
	fclose(fd);

	return TRUE;
}
/********************************************************************************/
static void read_nwchem_file_dlg()
{
	GtkWidget* filesel = 
 	file_chooser_open(read_nwchem_file,
			_("Read last frequencies and intensities from a NWChem output file"),
			GABEDIT_TYPEFILE_NWCHEM,GABEDIT_TYPEWIN_OTHER);

	gtk_window_set_modal (GTK_WINDOW (filesel), TRUE);
}
/********************************************************************************/
static gboolean read_psicode_file(GabeditFileChooser *SelecFile, gint response_id)
{
	gchar *FileName;
 	gchar t[BSIZE];
 	gchar sdum1[BSIZE];
 	gboolean OK;
 	FILE *fd;
 	guint taille=BSIZE;
	gint numberOfFrequencies = 0;
	gdouble *frequencies = NULL;
	gdouble *intensities = NULL;

	if(response_id != GTK_RESPONSE_OK) return FALSE;
 	FileName = gabedit_file_chooser_get_current_file(SelecFile);

 	fd = FOpen(FileName, "rb");
	if(!fd) return FALSE;

 	do 
 	{
 		OK=FALSE;
		while(!feof(fd) )
                {
                        if(!fgets(t,taille,fd)) break;
                        if(strstr(t,"Projected Infra Red Intensities"))
                        {
				numberOfFrequencies = 0;
				if(frequencies) g_free(frequencies);
				if(intensities) g_free(intensities);
				frequencies = NULL;
				intensities = NULL;
                                if(!fgets(t,taille,fd)) break;
                                if(!fgets(t,taille,fd)) break;
                                 while(!feof(fd) )
                                {
                                        if(!fgets(t,taille,fd)) break;
					if(strstr(t,"---------"))break;
					numberOfFrequencies++;
					frequencies = g_realloc(frequencies, numberOfFrequencies*sizeof(gdouble));
					intensities = g_realloc(intensities, numberOfFrequencies*sizeof(gdouble));
                                        sscanf(t,"%s %lf %s %s %s %lf", sdum1,&frequencies[numberOfFrequencies-1],sdum1,sdum1,sdum1, &intensities[numberOfFrequencies-1]);
                                }
				OK = TRUE;
                        }
                }
		if(!OK) break;
 	}while(!feof(fd));

	if(numberOfFrequencies>0)
	{
		createIRSpectrumWin(numberOfFrequencies, frequencies, intensities);
	}
	else
	{
		messageErrorFreq(FileName);
	}


	if(frequencies) g_free(frequencies);
	if(intensities) g_free(intensities);
	fclose(fd);

	return TRUE;
}
/********************************************************************************/
static void read_psicode_file_dlg()
{
	GtkWidget* filesel = 
 	file_chooser_open(read_psicode_file,
			_("Read last frequencies and intensities from a Psicode output file"),
			GABEDIT_TYPEFILE_PSICODE,GABEDIT_TYPEWIN_OTHER);

	gtk_window_set_modal (GTK_WINDOW (filesel), TRUE);
}
/********************************************************************************/
static gboolean read_adf_file(GabeditFileChooser *SelecFile, gint response_id)
{
 	gchar t[BSIZE];
 	gchar sdum1[BSIZE];
 	gchar sdum2[BSIZE];
 	gchar sdum3[BSIZE];
 	gboolean OK;
 	guint taille=BSIZE;
	gint i;
	gint k;
	gint ne;
	gint nf;
	gdouble freq[5];
	gint numberOfFrequencies = 0;
	gdouble* frequencies = NULL;
	gdouble* intensities = NULL;
	gdouble a1,b1,c1,a2,b2,c2,a3,b3,c3;

	gchar *FileName;
 	FILE *fd;

	if(response_id != GTK_RESPONSE_OK) return FALSE;
 	FileName = gabedit_file_chooser_get_current_file(SelecFile);

 	fd = FOpen(FileName, "rb");
	if(!fd)
	{
		messageErrorFreq(FileName);
		if(frequencies) g_free(frequencies);
		if(intensities) g_free(intensities);
		return FALSE;
	}

 	OK=FALSE;
 	while(!feof(fd))
	{
    		if(!feof(fd)) { char* e = fgets(t,BSIZE,fd);}
		/* printf("%s",t);*/
	 	if ( strstr( t,"Vibrations and Normal Modes") )
	  	{
			OK = TRUE;
			break;
	  	}
	}
	if(OK)
	{
 		OK=FALSE;
		if(fgets(t,taille,fd)) 
		if(fgets(t,taille,fd))
		if(fgets(t,taille,fd))
		if(fgets(t,taille,fd)) OK = TRUE;
	}
	if(!OK)
	{
		messageErrorFreq(FileName);
		if(frequencies) g_free(frequencies);
		if(intensities) g_free(intensities);
		return FALSE;
	}

	numberOfFrequencies = 0;
  	while(!feof(fd))
  	{
    		if(!feof(fd)) { char* e = fgets(t,BSIZE,fd);}
    		if(!feof(fd)) { char* e = fgets(t,BSIZE,fd);}
		if(!fgets(t,taille,fd))
		{
			messageErrorFreq(FileName);
			if(frequencies) g_free(frequencies);
			if(intensities) g_free(intensities);
			return FALSE;
		}

		nf = sscanf(t,"%s %lf %lf %lf", sdum1, &freq[0],&freq[1],&freq[2]);
		nf -= 1;
		if(nf<1) break;

    		if(!fgets(t,taille,fd)) break;

		for(k=0;k<nf;k++)
		{
			numberOfFrequencies++;
			frequencies = g_realloc(frequencies, numberOfFrequencies*sizeof(gdouble));
			intensities = g_realloc(intensities, numberOfFrequencies*sizeof(gdouble));
			frequencies[numberOfFrequencies-1] = freq[k];
			intensities[numberOfFrequencies-1] = 0;
		}
		while(!feof(fd))
		{
			long pos =  ftell(fd);
    			if(!fgets(t,taille,fd)) break;
			ne = sscanf(t,"%s %s %lf %lf %lf %lf %lf %lf %lf %lf %lf",sdum1,sdum2,&a1,&b1,&c1,&a2,&b2,&c2,&a3,&b3,&c3);
			ne -=2;
			if(ne!=nf*2)
			{
				fseek(fd, pos, SEEK_SET);
				break;
			}
		}
	}
	OK = FALSE;
 	while(!feof(fd))
	{
    		if(!feof(fd)) { char* e = fgets(t,BSIZE,fd);}
	 	if ( strstr( t,"Frequency") && strstr( t,"Dipole Strength") )
	  	{
			OK = TRUE;
			break;
	  	}
	}
	if(OK)
	{
    		if(!feof(fd)) { char* e = fgets(t,BSIZE,fd);}
    		if(!feof(fd)) { char* e = fgets(t,BSIZE,fd);}
		for(i=0;i<vibration.numberOfFrequencies;i++)
		{
    			if(!fgets(t,taille,fd)) break;
			ne = sscanf(t,"%s %s %s %lf",sdum1,sdum2, sdum3,&intensities[i]);
			if(ne<3)break;
		}
	}

	if(numberOfFrequencies>0 && OK)
	{
		createIRSpectrumWin(numberOfFrequencies, frequencies, intensities);
	}
	else
	{
		messageErrorFreq(FileName);
		if(frequencies) g_free(frequencies);
		if(intensities) g_free(intensities);
		return FALSE;
	}

	if(frequencies) g_free(frequencies);
	if(intensities) g_free(intensities);
	return TRUE;
}
/********************************************************************************/
static void read_adf_file_dlg()
{
	GtkWidget* filesel = 
 	file_chooser_open(read_adf_file,
			_("Read last frequencies and intensities from a ADF output file"),
			GABEDIT_TYPEFILE_MOLPRO,GABEDIT_TYPEWIN_OTHER);

	gtk_window_set_modal (GTK_WINDOW (filesel), TRUE);
}
/********************************************************************************/
static gboolean read_orca_file(GabeditFileChooser *SelecFile, gint response_id)
{

	gchar *FileName;
 	gchar t[BSIZE];
 	gchar sdum1[BSIZE];
 	gboolean OK;
 	FILE *fd;
 	guint taille=BSIZE;
	gint n;
	gdouble freq = 0;
	gdouble IRIntensity = 0;
	gint numberOfFrequencies = 0;
	gdouble *frequencies = NULL;
	gdouble *intensities = NULL;

	if(response_id != GTK_RESPONSE_OK) return FALSE;
 	FileName = gabedit_file_chooser_get_current_file(SelecFile);

 	fd = FOpen(FileName, "rb");
	if(!fd) return FALSE;

 	do 
 	{
 		OK=FALSE;
 		while(!feof(fd))
		{
    			if(!feof(fd)) { char* e = fgets(t,BSIZE,fd);}
	 		if (strstr( t,"IR SPECTRUM") ) OK = TRUE;
	 		if (strstr( t,"TX")  && strstr( t,"TY") && strstr( t,"TZ") && OK ){ OK = TRUE; break;}
		}
		if(!OK) break;
		numberOfFrequencies = 0;
		if(frequencies) g_free(frequencies);
		if(intensities) g_free(intensities);
		frequencies = NULL;
		intensities = NULL;
    		if(!feof(fd)) { char* e = fgets(t,BSIZE,fd);}
  		while(!feof(fd) )
  		{
			if(!fgets(t,taille,fd)) break;
			if(atoi(t)<=0) break;
			n = sscanf(t,"%s %lf %lf", sdum1, &freq,&IRIntensity);
			if(n==3)
			{
				numberOfFrequencies++;
				frequencies = g_realloc(frequencies, numberOfFrequencies*sizeof(gdouble));
				intensities = g_realloc(intensities, numberOfFrequencies*sizeof(gdouble));
				frequencies[numberOfFrequencies-1] = freq;
				intensities[numberOfFrequencies-1] = IRIntensity;
			}
		}
 	}while(!feof(fd));

	if(numberOfFrequencies>0)
	{
		createIRSpectrumWin(numberOfFrequencies, frequencies, intensities);
	}
	else
	{
		messageErrorFreq(FileName);
	}


	if(frequencies) g_free(frequencies);
	if(intensities) g_free(intensities);
	fclose(fd);

	return TRUE;
}
/********************************************************************************/
static void read_orca_file_dlg()
{
	GtkWidget* filesel = 
 	file_chooser_open(read_orca_file,
			_("Read last frequencies and intensities from a Orca output file"),
			GABEDIT_TYPEFILE_ORCA,GABEDIT_TYPEWIN_OTHER);

	gtk_window_set_modal (GTK_WINDOW (filesel), TRUE);
}
/********************************************************************************/
static gboolean read_sample_2columns_file(GabeditFileChooser *SelecFile, gint response_id)
{
 	gchar t[BSIZE];
 	gboolean OK = TRUE;
	gint numberOfFrequencies = 0;
	gdouble* frequencies = NULL;
	gdouble* intensities = NULL;
	gchar *FileName;
 	FILE *fd;
	gdouble a;
	gdouble b;
	int ne = 0;

	if(response_id != GTK_RESPONSE_OK) return FALSE;
 	FileName = gabedit_file_chooser_get_current_file(SelecFile);

 	fd = FOpen(FileName, "rb");
	if(!fd) return FALSE;

 	while(!feof(fd))
	{
	 	if(!fgets(t,BSIZE,fd))break;
		ne = sscanf(t,"%lf %lf",&a,&b);
		if(ne==2)
		{
			numberOfFrequencies++;
			frequencies = g_realloc(frequencies, numberOfFrequencies*sizeof(gdouble));
			intensities = g_realloc(intensities, numberOfFrequencies*sizeof(gdouble));
			frequencies[numberOfFrequencies-1] = a;
			intensities[numberOfFrequencies-1] = b;
		}
 	}

	if(numberOfFrequencies>0)
	{
		createIRSpectrumWin(numberOfFrequencies, frequencies, intensities);
	}
	else
	{
		OK = FALSE;
		messageErrorFreq(FileName);
	}


	if(frequencies) g_free(frequencies);
	if(intensities) g_free(intensities);
	fclose(fd);
	return OK;
}
/********************************************************************************/
static void read_sample_2columns_file_dlg()
{
	GtkWidget* filesel = 
 	file_chooser_open(read_sample_2columns_file,
			_("Read frequencies and intensities from a sample file(2columns : first = Energy(cm-1), second = intensity)"),
			GABEDIT_TYPEFILE_TXT,GABEDIT_TYPEWIN_OTHER);

	gtk_window_set_modal (GTK_WINDOW (filesel), TRUE);
}
/********************************************************************************/
static gboolean read_igvpt2_file(GabeditFileChooser *SelecFile, gint response_id)
{
 	gchar t[BSIZE];
 	gboolean OK = TRUE;
	gint numberOfFrequencies = 0;
	gdouble* frequencies = NULL;
	gdouble* intensities = NULL;
	gchar *fileName;
	gdouble a;
	gdouble b;
	int ne = 0;
	FILE* file;
	gchar* result;
	gchar* old;

	if(response_id != GTK_RESPONSE_OK) return FALSE;
 	fileName = gabedit_file_chooser_get_current_file(SelecFile);
 	file = fopen(fileName, "r");
	if(file)
	{
		gchar* t = g_malloc(BSIZE*sizeof(gchar));
		result = g_strdup_printf("%s","");
		while(!feof(file))
        	{
                	if(!fgets(t,BSIZE,file))break;
			old = result;
			result = g_strdup_printf("%s%s",old,t);
                        if(old) g_free(old);
        	}
        	if(result)
        	{
                	GtkWidget* message = AnharmonicResultTxt(result,"iGVPT2 result");
                	gtk_window_set_default_size (GTK_WINDOW(message),(gint)(ScreenWidth*0.8),-1);
                	gtk_widget_set_size_request(message,(gint)(ScreenWidth*0.45),-1);
                	/* gtk_window_set_modal (GTK_WINDOW (message), TRUE);*/
                	/* gtk_window_set_transient_for(GTK_WINDOW(message),GTK_WINDOW(PrincipalWindow));*/
        	}
		fclose(file);
		g_free(t);
	}


	return OK;
}
/********************************************************************************/
static void read_igvpt2_file_dlg()
{
	GtkWidget* filesel = 
 	file_chooser_open(read_igvpt2_file,
			_("Read frequencies and intensities from an iGVPT2 output file"),
			GABEDIT_TYPEFILE_ORCA,GABEDIT_TYPEWIN_OTHER);

	gtk_window_set_modal (GTK_WINDOW (filesel), TRUE);
}
/********************************************************************************/
void createIRSpectrum(GtkWidget *parentWindow, GabEditTypeFile typeOfFile)
{
	if(typeOfFile==GABEDIT_TYPEFILE_GABEDIT) read_gabedit_file_dlg();
	if(typeOfFile==GABEDIT_TYPEFILE_MOLDEN) read_molden_file_dlg();
	if(typeOfFile==GABEDIT_TYPEFILE_MOLPRO) read_molpro_file_dlg();
	if(typeOfFile==GABEDIT_TYPEFILE_DALTON) read_dalton_file_dlg();
	if(typeOfFile==GABEDIT_TYPEFILE_GAMESS) read_gamess_file_dlg();
	if(typeOfFile==GABEDIT_TYPEFILE_GAMESS_ANHARMONIC) read_gamess_anharmonic_file_dlg();
	if(typeOfFile==GABEDIT_TYPEFILE_ORCA) read_orca_file_dlg();
	if(typeOfFile==GABEDIT_TYPEFILE_FIREFLY) read_gamess_file_dlg();
	if(typeOfFile==GABEDIT_TYPEFILE_GAUSSIAN) read_gaussian_file_dlg();
	if(typeOfFile==GABEDIT_TYPEFILE_GAUSSIAN_ANHARMONIC) read_gaussian_anharmonic_file_dlg();
	if(typeOfFile==GABEDIT_TYPEFILE_NWCHEM) read_nwchem_file_dlg();
	if(typeOfFile==GABEDIT_TYPEFILE_PSICODE) read_psicode_file_dlg();
	if(typeOfFile==GABEDIT_TYPEFILE_QCHEM) read_qchem_file_dlg();
	if(typeOfFile==GABEDIT_TYPEFILE_ADF) read_adf_file_dlg();
	if(typeOfFile==GABEDIT_TYPEFILE_MPQC) read_mpqc_file_dlg();
	if(typeOfFile==GABEDIT_TYPEFILE_TXT) read_sample_2columns_file_dlg();
	if(typeOfFile==GABEDIT_TYPEFILE_IGVPT2) read_igvpt2_file_dlg();
}
/********************************************************************************/
void createIRSpectrumFromVibration(GtkWidget *parentWindow, Vibration ibration)
{
	gint numberOfFrequencies = vibration.numberOfFrequencies;
	gdouble* frequencies = NULL;
	gdouble* intensities = NULL;
	gint j;
	if(numberOfFrequencies<1)
	{
		GtkWidget* mess=Message(_("For draw spectrum,Sorry, Please read a file before"),_("Error"),TRUE);
    		gtk_window_set_modal (GTK_WINDOW (mess), TRUE);
		return;
	}
	frequencies = g_malloc(numberOfFrequencies*sizeof(gdouble));
	intensities = g_malloc(numberOfFrequencies*sizeof(gdouble));

	for (j=0; j < vibration.numberOfFrequencies; j++)
	{
		frequencies[j] = vibration.modes[j].frequence;
		intensities[j] = vibration.modes[j].IRIntensity;
	}
	createIRSpectrumWin(numberOfFrequencies, frequencies, intensities);
	if(frequencies) g_free(frequencies);
	if(intensities) g_free(intensities);
}
