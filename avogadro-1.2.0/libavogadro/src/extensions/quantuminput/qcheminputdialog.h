/**********************************************************************
  QChemInputDialog - Dialog for generating Q-Chem input decks

  Copyright (C) 2008-2009 Marcus D. Hanwell

  This file is part of the Avogadro molecular editor project.
  For more information, see <http://avogadro.cc/>

  Avogadro is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  Avogadro is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
  02110-1301, USA.
 **********************************************************************/

#ifndef QCHEMINPUTDIALOG_H
#define QCHEMINPUTDIALOG_H

#include "inputdialog.h"
#include "ui_qcheminputdialog.h"

namespace Avogadro
{
  class Molecule;
  class QChemInputDialog : public InputDialog
  {
  Q_OBJECT

  public:
    explicit QChemInputDialog(QWidget *parent = 0, Qt::WindowFlags f = 0 );
    ~QChemInputDialog();

    void setMolecule(Molecule *molecule);
    void readSettings(QSettings&);
    void writeSettings(QSettings&) const;

    enum calculationType{SP, OPT, FREQ};
    enum theoryType{RHF, MP2, B3LYP, B3LYP5, EDF1, M062X, CCSD};
    enum basisType{STO3G, B321G, B631Gd, B631Gdp, B631plusGd, B6311Gd, ccpVDZ, ccpVTZ, LANL2DZ, LACVP};

  protected:
    /**
     * Reimplemented to update the dialog when it is shown
     */
    void showEvent(QShowEvent *event);

  private:
    Ui::QChemInputDialog ui;
//    Molecule* m_molecule;

    // Internal data structure for the calculation
    //QString m_title;
    calculationType m_calculationType;
    theoryType m_theoryType;
    basisType m_basisType;
    //int m_multiplicity;
    //int m_charge;
    QString m_output;
    coordType m_coordType;
    bool m_dirty;
    bool m_warned;

    // Generate an input deck as a string
    QString generateInputDeck();
    // Translate enums to strings
    QString getCalculationType(calculationType t);
    QString getTheoryType(theoryType t);
    QString getBasisType(basisType t);

    // Enable/disable form elements
    void deckDirty(bool);

  public Q_SLOTS:
    void updatePreviewText();

  private Q_SLOTS:
    //! Button Slots
    void resetClicked();
    void generateClicked();
    void enableFormClicked();
    void moreClicked();
    void previewEdited();

    void setTitle();
    void setCalculation(int);
    void setTheory(int);
    void setBasis(int);
    void setMultiplicity(int);
    void setCharge(int);
    void setCoords(int);
  };
}

#endif
